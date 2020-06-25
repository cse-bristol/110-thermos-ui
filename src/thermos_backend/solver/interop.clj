(ns thermos-backend.solver.interop
  (:require [clojure.java.io :as io]
            [thermos-backend.util :as util]
            [clojure.tools.logging :as log]
            [clojure.java.shell :refer [sh]]
            [clojure.edn :as edn]
            
            [clojure.data.json :as json]
            [clojure.set :as set]
            [clojure.string]

            [loom.graph :as graph]
            [loom.alg :as graph-alg]
            [loom.attr :as attr]
            
            [thermos-specs.document :as document]
            [thermos-specs.solution :as solution]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.supply :as supply]
            [thermos-specs.path :as path]
            [thermos-backend.solver.bounds :as bounds]
            [thermos-backend.config :refer [config]]
            [thermos-util :refer [kw->annual-kwh
                                  annual-kwh->kw
                                  format-seconds
                                  safe-div]]
            [thermos-util.finance :as finance]
            [thermos-util.pipes :as pipes]
            [clojure.walk :refer [postwalk]]

            [thermos-specs.tariff :as tariff]
            [thermos-specs.measure :as measure]
            [thermos-backend.solver.market :as market]
            [clojure.string :as string]

            [thermos.opt.net.core :as net-model]
            )
  
  (:import [java.io StringWriter]))

(def HOURS-PER-YEAR 8766)

(defn create-graph [buildings paths]
  (apply
   graph/graph
   (concat
    (mapcat ::candidate/connections buildings)

    (map ::path/start paths)
    (map ::path/end paths)
    
    (map ::candidate/id buildings)

    (map #(vector (::path/start %) (::path/end %)) paths)
    (mapcat #(for [c (::candidate/connections %)] [c (::candidate/id %)]) buildings))))

(defn- check-invalid-edges [net-graph at]
  (doseq [[i j] (graph/edges net-graph)]
    (when (or (nil? i) (nil? j))
      (throw (ex-info "Invalid edge found in graph" {:i i :j j :at at}))))
  net-graph)

(defn- simplify-topology
  "For CANDIDATES, create a similar graph in which all vertices of degree two
  that don't represent demand or supply points have been collapsed.

  You should restrict CANDIDATES to the included candidates first.

  The output graph has edge labels :ids, which relate to a collection of
  candidate path IDs that are included by using that edge.
  "
  [candidates]

  (let [{paths :path buildings :building}
        (group-by ::candidate/type candidates)

        net-graph (check-invalid-edges
                   (create-graph buildings paths)
                   :construction)

        _ (log/info "Basic graph has" (count (graph/nodes net-graph)) "nodes and" (count (graph/edges net-graph)) "edges")
        
        ;; tag all the real vertices
        net-graph (reduce (fn [g d]
                            (attr/add-attr g (::candidate/id d) :real-vertex true))
                          net-graph
                          buildings)

        ;; tag all the edges with their path IDs and cost parameters
        net-graph (reduce (fn [g p]
                            
                            (-> g
                                ;; the variable and fixed cost
                                ;; parameters are merged into the path
                                ;; by code later on. Within a normal
                                ;; candidate they would be a reference
                                ;; to something in the document.

                                (attr/add-attr (::path/start p) (::path/end p)
                                               :ids #{(::candidate/id p)})

                                (attr/add-attr (::path/start p)
                                               (::path/end p)
                                               :exists (::path/exists p))
                                
                                (attr/add-attr (::path/start p) (::path/end p)
                                               :variable-cost (::path/variable-cost p 0))
                                (attr/add-attr (::path/start p) (::path/end p)
                                               :fixed-cost (::path/fixed-cost p 0))))
                          net-graph
                          paths)

        ;; delete all self-edges
        net-graph (check-invalid-edges
                   (graph/remove-edges*
                    net-graph
                    (filter (fn [e] (= (second e) (first e)))
                            (graph/edges net-graph)))
                   :delete-self-edges)

        combinable-costs?
        (fn [net-graph v]
          (let [[e1 e2] (graph/out-edges net-graph v)
                variable-cost-1 (or (attr/attr net-graph e1 :variable-cost) 0)
                variable-cost-2 (or (attr/attr net-graph e2 :variable-cost) 0)
                length-1 (or (attr/attr net-graph e1 :length) 0)
                length-2 (or (attr/attr net-graph e2 :length) 0)]
            (or (= variable-cost-1 variable-cost-2)
                (zero? variable-cost-1)
                (zero? variable-cost-2)
                (zero? length-1)
                (zero? length-2))))
        
        collapsible? (fn [net-graph node]
                       (and (not (attr/attr net-graph node :real-vertex))
                            (= 2 (graph/out-degree net-graph node))
                            (combinable-costs? net-graph node)))
        

        collapse-junction
        ;; this is a function to take a graph and delete a node,
        ;; preserving the identiy information on the edges. This will
        ;; later let us emit the edges into the output usefully.
        (fn [graph node]
          (if (collapsible? graph node)
            (let [edges (graph/out-edges graph node)
                  all-ids (set (mapcat #(attr/attr graph % :ids) edges))
                  ;; since an edge is just a tuple, apply concat edges gives me
                  ;; e.g. [a b] [b c] => [a b c]
                  ;; we only call this when there are exactly two edges
                  ;; so deleting b gives us [a c] which is our new edge
                  new-edge (vec (remove (partial = node) (apply concat edges)))

                  graph (-> graph
                            (graph/remove-nodes node)
                            (graph/add-edges new-edge))

                  existing-ids (attr/attr graph new-edge :ids)
                  ]
              (if (empty? existing-ids)
                (attr/add-attr graph new-edge :ids all-ids)
                graph))

            ;; if we in the reduce phase below made the node not collapsible any more,
            ;; don't try and collapse it after all.
            graph))
        
        ;; collapse all collapsible edges until we have finished doing so.
        net-graph
        (check-invalid-edges
         (loop [net-graph net-graph]
           (let [collapsible (->> (graph/nodes net-graph) (filter #(collapsible? net-graph %)))]
             (if (empty? collapsible)
               net-graph

               ;; Although removing one collapsible junction may
               ;; render another one invalid in the case where there
               ;; is a loop in the graph, so (recur) sounds unsafe, we
               ;; have a check in collapse-junction which covers this
               ;; off. We might be able to make the loop more
               ;; efficient in principle by determining which new
               ;; collapsible vertices have appeared as we go, but why
               ;; bother.
               (recur (reduce collapse-junction net-graph collapsible)))))
         :collapse-junctions)
        

        ;; prune all spurious junctions
        net-graph
        
        (check-invalid-edges
         (loop [net-graph net-graph]
           (let [spurious (->> (graph/nodes net-graph)
                               (filter #(not (attr/attr net-graph % :real-vertex)))
                               (filter #(= 1 (graph/out-degree net-graph %))))]
             (if (empty? spurious)
               net-graph
               (recur (graph/remove-nodes* net-graph spurious)))))
         :prune)]
    
    (log/info "Simplified graph has" (count (graph/nodes net-graph)) "nodes and" (count (graph/edges net-graph)) "edges")
    net-graph))

(defn- summarise-attributes
  "Take CANDIDATES and NET-GRAPH being a loom graph with :ids on some edges,
  and add :length :requirement onto those edges being the total length
  and requirement of corresponding paths in CANDIDATES

  The total requirement is required if one part is required."
  [net-graph candidates]
  
  (let [paths
        (->> candidates
             (filter candidate/is-path?)
             (map #(vector (::candidate/id %) %))
             (into {}))

        combine-cost
        (fn [path-ids cost]
          (let [lengths (map (comp ::path/length paths) path-ids)
                costs   (map (comp cost paths) path-ids)
                total-l (reduce + lengths)
                total-c (reduce + (map * lengths costs))]
            (if (zero? total-c)
              0.0
              (/ total-c total-l))))
        
        total-value
        (fn [path-ids value]
          (let [costs (map #(or (value (paths %)) 0) path-ids)]
            (apply + costs)))
        
        total-requirement
        (fn [path-ids]
          (let [requirements (map (comp ::candidate/inclusion paths) path-ids)
                result (if (some #{:required} requirements)
                         true false)]
            result))

        total-max-dia
        (fn [path-ids]
          (let [max-dias (keep (comp ::path/maximum-diameter paths) path-ids)]
            (when (not-empty max-dias) (reduce min max-dias))))
        ]

    (reduce (fn [g e]
              (let [path-ids (attr/attr g e :ids)]
                (cond-> g
                  (seq path-ids)
                  (-> (attr/add-attr e :length (total-value path-ids ::path/length))
                      (attr/add-attr e :required (total-requirement path-ids))
                      (attr/add-attr e :max-dia (total-max-dia path-ids))
                      (attr/add-attr e :fixed-cost (combine-cost path-ids ::path/fixed-cost))
                      (attr/add-attr e :variable-cost (combine-cost path-ids ::path/variable-cost))))))
            
            net-graph (graph/edges net-graph))))

(defn- demand-value-terms
  "Given a vertex, calculate the value terms for it. These are value, value/kwh, and value/kwp.
  They are needed separately to allow the optimiser to think about demand reduction measures."
  [instance candidate market]

  (let [ignore-revenues (= :system (::document/objective instance :network))
        mode (document/mode instance)
        
        kwh (float   (candidate/annual-demand candidate mode))
        kwp (float   (max (candidate/peak-demand candidate mode)
                          (annual-kwh->kw kwh)))

        {standing-charge     ::tariff/standing-charge
         unit-charge         ::tariff/unit-charge
         capacity-charge     ::tariff/capacity-charge}
        (if (= :market (::tariff/id candidate))
          (market (::candidate/id candidate))
          (document/tariff-for-id instance (::tariff/id candidate)))

        {fixed-connection    ::tariff/fixed-connection-cost
         variable-connection ::tariff/variable-connection-cost}
        (document/connection-cost-for-id instance (::tariff/cc-id candidate))

        standing-charge (if ignore-revenues 0 (or standing-charge 0))
        unit-charge     (if ignore-revenues 0 (or unit-charge 0))
        capacity-charge (if ignore-revenues 0 (or capacity-charge 0))

        fixed-connection (or fixed-connection 0)
        variable-connection (or variable-connection 0)
        ]
    {:kwh        kwh
     :kwp        kwp
     :value      (float (- (finance/objective-value instance
                                                    :heat-revenue
                                                    standing-charge)
                           (finance/objective-value instance
                                                    :connection-capex
                                                    fixed-connection)))
     
     :value%kwh (float (finance/objective-value instance :heat-revenue unit-charge))
     :value%kwp (float (- (finance/objective-value instance :heat-revenue capacity-charge)
                           (finance/objective-value instance :connection-capex variable-connection)))}))

(defn- insulation-max-area [measure candidate]
  (let [area (get candidate
                  (case (::measure/surface measure)

                    :roof  ::candidate/roof-area
                    :floor ::candidate/ground-area
                    
                    ::candidate/wall-area)
                  0)]
    (* area (::measure/maximum-area measure 0))))

(defn- insulation-max-effect [measure base-demand]
  (* (::measure/maximum-effect measure 0) base-demand))

(defn- insulation-definitions [instance candidate]
  (let [mode (document/mode instance)
        base-demand (candidate/annual-demand candidate mode)]
    {:insulation
     (->> (when (::document/consider-insulation instance)
            (for [insulation-id (::demand/insulation candidate)
                  :let [measure (get-in instance [::document/insulation insulation-id])
                        area (insulation-max-area measure candidate)
                        maximum-kwh-saved (insulation-max-effect measure base-demand)]
                  :when (and measure
                             (pos? area)
                             (pos? maximum-kwh-saved))]
              (let [cost-per-m2 (::measure/cost-per-m2 measure 0)
                    maximum-cost (* area cost-per-m2)
                    cost-per-kwh (safe-div maximum-cost maximum-kwh-saved)]
                {:id insulation-id
                 :cost      (finance/objective-value instance
                                                     :insulation-capex
                                                     (::measure/fixed-cost measure 0))
                 :cost%kwh (finance/objective-value instance
                                                    :insulation-capex
                                                    cost-per-kwh)
                 :minimum   (if (::document/force-insulation instance) maximum-kwh-saved 0)
                 :maximum   maximum-kwh-saved})))
          (filter identity))}))

(defn- alternative-definitions [instance candidate]
   {:alternatives
    (let [counterfactual (::demand/counterfactual candidate)
          ids (set
               (conj
                (when (::document/consider-alternatives instance)
                  (::demand/alternatives candidate))
                counterfactual))]
      
      (->>
       (for [id ids]
         (when-let [alternative (get-in instance [::document/alternatives id])]
           
           (let [capex-type (if (= counterfactual id)
                             :counterfactual-capex
                             :alternative-capex)]
             {:id id
              :cost      (finance/objective-value instance capex-type
                                                  (::supply/fixed-cost alternative 0))


              ;; we pay for fuel for the counterfactual
              :cost%kwh (+
                          (finance/objective-value instance capex-type
                                                   (annual-kwh->kw
                                                    (::supply/capex-per-mean-kw alternative 0)))
                          (finance/objective-value instance :alternative-opex
                                                   (::supply/cost-per-kwh alternative 0)))
              
              :cost%kwp
              ;; we pay opex for the CF unless in network-only mode.
              (+ (finance/objective-value instance :alternative-opex
                                          (::supply/opex-per-kwp alternative 0))

                 (finance/objective-value instance capex-type
                                          (::supply/capex-per-kwp alternative 2)))
              
              :emissions
              (into {}
                    (for [e candidate/emissions-types]
                      [e (float (get-in alternative [::supply/emissions e] 0))]))})))
       (filter identity)))})

(defn demand-terms [instance candidate market]
  (merge
   {:required  (boolean (candidate/required? candidate))
    :count     (int     (::demand/connection-count candidate 1))}
   (demand-value-terms instance candidate market)

   ;; we only offer insulation & alternatives to the optimiser in system mode.
   (when (= :system (::document/objective instance :network))
     (insulation-definitions instance candidate))
   (when (= :system (::document/objective instance :network))
     (alternative-definitions instance candidate))))

(defn- supply-terms [instance candidate]
  (let [fixed-cost
        (-> candidate
            (::supply/fixed-cost 0)
            (->> (finance/objective-value instance :supply-capex)))

        capex-per-kwp
        (-> candidate
            (::supply/capex-per-kwp 0)
            (->> (finance/objective-value instance :supply-capex)))

        heat-cost-per-kwh
        (-> candidate
            (::supply/cost-per-kwh 0)
            (->> (finance/objective-value instance :supply-heat)))

        opex-per-kwp
        (-> candidate
            (::supply/opex-per-kwp 0)
            (->> (finance/objective-value instance :supply-opex)))

        pumping-overhead
        (::document/pumping-overhead instance 0.0)

        pumping-cost-per-kwh
        (::document/pumping-cost-per-kwh instance 0.0)

        pumping-emissions
        (::document/pumping-emissions instance {})

        is-cooling (document/is-cooling? instance)

        adjust-for-pumping
        (fn [main-factor pumping-factor]
          (+ (* pumping-overhead pumping-factor)
             (* main-factor (if is-cooling
                              (+ 1 pumping-overhead)
                              (- 1 pumping-overhead)))))
        
        effective-heat-cost
        (adjust-for-pumping heat-cost-per-kwh pumping-cost-per-kwh)

        emissions-factors
        (into {}
              (for [e candidate/emissions-types]
                [e (float
                    (adjust-for-pumping
                     (get-in candidate [::supply/emissions e] 0)
                     (get pumping-emissions e 0)))]))
        ]
    
    {:capacity-kw (float (::supply/capacity-kwp  candidate 0))
     :cost        (float fixed-cost)
     :cost%kwh    (float effective-heat-cost)
     :cost%kwp    (float (+ capex-per-kwp opex-per-kwp))
     
     :emissions   emissions-factors}))

(defn- edge-terms [instance cost-function bounds net-graph edge]
  (let [length (or (attr/attr net-graph edge :length) 0)

        [fixed-cost variable-cost]
        (if (zero? length) [0 0] ;; zero length path has no cost, and is probably a virtual connecty thing
            (let [{[[lf lb] [uf ub]] :mean} bounds]
              (cost-function (if (zero? (min lf lb))
                               (max lf lb)
                               (min lf lb))
                             (max uf ub)
                             (or (attr/attr net-graph edge :fixed-cost)
                                 (log/error "Missing fixed cost for" edge)
                                 0)
                             
                             (or (attr/attr net-graph edge :variable-cost)
                                 (log/error "Missing variable cost for" edge)
                                 0))))

        cost-type (if (attr/attr net-graph edge :exists)
                    :existing-pipe-capex
                    :pipe-capex)]

    (when (and (seq (attr/attr net-graph edge :ids))
               (not (attr/attr net-graph edge :length)))
      (log/error "Edge" edge "which maps to real edges"
                 (attr/attr net-graph edge :ids)
                 "has no recorded length"))

    {:i (first edge) :j (second edge)
     :length    length
     :cost%m    (float (finance/objective-value instance cost-type fixed-cost))
     :cost%kwm (float (finance/objective-value instance cost-type variable-cost))
     :bounds bounds
     :required (boolean (attr/attr net-graph edge :required))}))

(defn- instance->json [instance net-graph power-curve market]
  (let [candidates     (::document/candidates instance)

        mode (document/mode instance)
        
        edge-bounds (do
                      (log/info "Computing flow bounds...")
                      (bounds/edge-bounds
                       net-graph
                       :capacity (memoize (comp #(::supply/capacity-kwp % 0) candidates))
                       :demand
                       (memoize (fn [id]
                                  (-> (get candidates id)
                                      (candidate/annual-demand mode)
                                      (or 0.0)
                                      (annual-kwh->kw))))
                       :peak-demand
                       (memoize (fn [id]
                                  (-> (get candidates id)
                                      (candidate/peak-demand mode)
                                      (or 0.0))))
                       
                       :edge-max (let [inverse-power-curve (vec (map (comp vec reverse) power-curve))
                                       global-max (first (last power-curve))]
                                   (memoize
                                    (fn [i j]
                                      (min
                                       global-max
                                       (or (when-let [max-dia (attr/attr net-graph [i j] :max-dia)]
                                             (pipes/linear-evaluate inverse-power-curve max-dia))
                                           global-max)))))
                       
                       :size (comp #(::connection-count % 1) candidates)))

        
        _ (log/info "Computed flow bounds")
        ]
    
    {:time-limit      (float (::document/maximum-runtime instance 1.0))
     :mip-gap         (float (::document/mip-gap instance 0.05))
     :iteration-limit (int   (::document/maximum-iterations instance 1000))
     :supply-limit    (when-let [l (::document/maximum-supply-sites instance)] (int l))
     
     :pipe-losses
     (let [losses (pipes/heat-loss-curve
                   power-curve
                   (::document/flow-temperature instance)
                   (::document/return-temperature instance)
                   (::document/ground-temperature instance))]
       {:kwp  (map (comp float first) losses)
        :w%m  (map (comp float second) losses)})

     ;; global emissions costs and limits
     :emissions
     (into {}
           (for [e candidate/emissions-types]
             [e (merge {:cost    (float
                                  (finance/objective-value
                                   instance
                                   :emissions-cost
                                   (get-in instance [::document/emissions-cost e] 0)))}
                       (when     (get-in instance [::document/emissions-limit :enabled e])
                         {:limit (float (get-in instance [::document/emissions-limit :value e] 0))}))]))

     :vertices
     (for [vertex (graph/nodes net-graph)
           :let [candidate (candidates vertex)]
           :when (or (candidate/has-demand? candidate mode)
                     (candidate/has-supply? candidate))]
       (let [unreachable (attr/attr net-graph vertex :unreachable)]
         (cond-> {:id vertex}
           (candidate/has-demand? candidate mode)
           (assoc :demand (demand-terms instance candidate market))

           unreachable
           (assoc-in [:demand :required] false)

           (candidate/has-supply? candidate)
           (assoc :supply (supply-terms instance candidate)))))
     
     :edges
     (let [mech-A (::document/mechanical-cost-per-m instance 0.0)
           mech-B (::document/mechanical-cost-per-m2 instance 0.0)
           mech-C (::document/mechanical-cost-exponent instance 1.0)

           civil-C (::document/civil-cost-exponent instance 1.0)

           cost-function
           (fn [kw-min kw-max civil-fixed civil-var]
             (pipes/linear-cost-per-kw
              power-curve
              kw-min kw-max
              mech-A mech-B mech-C
              civil-fixed civil-var civil-C))]
       
       (for [edge (->> (graph/edges net-graph)
                       (map (comp vec sort))
                       (set))]
         (edge-terms instance cost-function (get edge-bounds edge) net-graph edge)))}))

(defn- index-by [f vs]
  (reduce #(assoc %1 (f %2) %2) {} vs))

(defn- output-insulation [instance candidate id kwh]
  (when (pos? kwh)
    (when-let [insulation (document/insulation-for-id instance id)]
      (let [mode        (document/mode instance)
            base-demand (candidate/annual-demand candidate mode)
            
            fixed-cost  (::measure/fixed-cost insulation 0)
            cost-per-m2 (::measure/cost-per-m2 insulation 0)
            max-area    (insulation-max-area insulation candidate)
            max-effect  (insulation-max-effect insulation base-demand)
            proportion-done (safe-div kwh max-effect)
            area-done   (* proportion-done max-area)
            cost        (+ fixed-cost (* cost-per-m2 area-done))]
        (merge
         {::measure/name (::measure/name insulation) ;; yes? no?
          ::measure/id   id
          :kwh kwh
          :area area-done
          :proportion proportion-done}
         (finance/adjusted-value instance :insulation-capex cost))))))

(defn- output-counterfactual [candidate instance]
  (assoc candidate
         ::solution/counterfactual
         (if-let [alternative (document/alternative-for-id instance (::demand/counterfactual candidate))]
           (let [cost-per-kwh (::supply/cost-per-kwh alternative 0)
                 opex-per-kwp (::supply/opex-per-kwp alternative 0)

                 kwh (candidate/annual-demand candidate (document/mode instance))
                 kwp (candidate/peak-demand candidate (document/mode instance))
                 
                 fuel (* cost-per-kwh kwh)
                 opex (* opex-per-kwp kwp)
                 ]
             {:opex (finance/adjusted-value instance :alternative-opex opex)
              :heat-cost (finance/adjusted-value instance :alternative-opex fuel)
              :emissions
              (into {}
                    (for [e candidate/emissions-types]
                      [e (finance/emissions-value
                          instance e
                          (* kwh (get-in alternative [::supply/emissions e] 0)))]))
              ::supply/id (::supply/id alternative)
              ::supply/name (::supply/name alternative)}))))

(defn- output-alternative [candidate instance alternative]
  (let [is-counterfactual (= (::demand/counterfactual candidate)
                             (::supply/id alternative))]
    (assoc candidate
           ::solution/alternative
           (if is-counterfactual
             (assoc (::solution/counterfactual candidate)
                    :counterfactual true)
             (let [kwh (::solution/kwh candidate)
                   kwp (candidate/peak-demand candidate (document/mode instance))

                   capex (supply/principal alternative kwp kwh)
                   opex  (supply/opex alternative kwp)
                   fuel  (supply/heat-cost alternative kwh)]
               {:capex (finance/adjusted-value instance :alternative-capex capex)
                :opex (finance/adjusted-value instance :alternative-opex opex)
                :heat-cost (finance/adjusted-value instance :alternative-opex fuel)
                :counterfactual false
                :emissions
                (into {}
                      (for [e candidate/emissions-types]
                        (let [alternative-factor (get (::supply/emissions alternative) e 0)
                              emissions (* alternative-factor kwh)]
                          [e (finance/emissions-value instance e emissions)])))
                ::supply/id (::supply/id alternative)
                ::supply/name (::supply/name alternative)})))))

(defn- merge-solution [instance net-graph power-curve market result-json]
  (let [state (keyword (:state result-json))
        
        solution-vertices (into {}
                                (for [v (:vertices result-json)]
                                  [(:id v) v]))

        solution-edges    (->> (:edges result-json)
                               (mapcat (fn [e]
                                         (map vector
                                              (attr/attr net-graph [(:i e) (:j e)] :ids)
                                              (repeat e))))
                               (into {}))

        update-vertex
        (fn [v]
          (let [solution-vertex (solution-vertices (::candidate/id v))
                tariff-id       (::tariff/id v)
                tariff          (if (= :market tariff-id)
                                  (market (::candidate/id v))
                                  (document/tariff-for-id instance tariff-id))

                connection-cost      (document/connection-cost-for-id instance (::tariff/cc-id v))
                
                insulation           (for [[id kwh] (:insulation solution-vertex)]
                                       (output-insulation instance v id kwh))

                total-insulation-kwh (reduce + 0 (map :kwh insulation))
                effective-demand     (- (candidate/annual-demand v (document/mode instance))
                                        total-insulation-kwh)

                alternative          (document/alternative-for-id
                                      instance
                                      (:alternative solution-vertex))

                effective-peak (candidate/peak-demand v (document/mode instance))
                
                heat-revenue
                (finance/adjusted-value
                 instance
                 :heat-revenue
                 (tariff/annual-heat-revenue tariff effective-demand effective-peak))

                connection-cost
                (finance/adjusted-value
                 instance
                 :connection-capex
                 (tariff/connection-cost connection-cost effective-demand effective-peak))
                ]
            (-> v
                (assoc ::solution/included true
                       ::solution/kwh effective-demand)

                ;; add on the counterfactual info
                (output-counterfactual instance)
                
                (cond-> 
                    ;; measures and alt systems
                  alternative
                  (output-alternative instance alternative)

                  ;; insulation needs costs working out
                  (pos? total-insulation-kwh)
                  (assoc ::solution/insulation insulation)
                  
                  ;; demand facts
                  (:connected solution-vertex)
                  (assoc ::solution/connected true
                         ::solution/heat-revenue heat-revenue
                         ::solution/connection-capex connection-cost)

                  (= :market tariff-id)
                  (assoc ::solution/market-rate
                         (::tariff/unit-charge tariff))
                  
                  ;; supply facts
                  (:capacity-kw solution-vertex)
                  (as-> out
                      (let [output-kwh  (:output-kwh solution-vertex)
                            capacity-kw (:capacity-kw solution-vertex)
                            diversity   (:diversity solution-vertex)

                            pumping-overhead     (::document/pumping-overhead instance 0.0)
                            pumping-cost-per-kwh (::document/pumping-cost-per-kwh instance 0.0)
                            pumping-emissions    (::document/pumping-emissions instance {})
                            pumping-kwh          (* output-kwh pumping-overhead)
                            is-cooling           (document/is-cooling? instance)

                             ;; +/- pumping overhead
                            output-kwh (if (document/is-cooling? instance)
                                         (+ output-kwh pumping-kwh)
                                         (- output-kwh pumping-kwh))]
                        
                        (assoc out
                               ::solution/capacity-kw   capacity-kw
                               ::solution/diversity     diversity
                               ::solution/output-kwh    output-kwh
                               ::solution/pumping-kwh   pumping-kwh

                               ::solution/supply-capex
                               (finance/adjusted-value
                                instance :supply-capex
                                (supply/principal out capacity-kw output-kwh))

                               ::solution/supply-opex
                               (finance/adjusted-value
                                instance :supply-opex
                                (supply/opex out capacity-kw))

                               ::solution/heat-cost
                               (finance/adjusted-value
                                instance
                                :supply-heat
                                (supply/heat-cost out output-kwh))

                               ::solution/pumping-cost
                               (finance/adjusted-value
                                instance
                                :supply-pumping
                                (* pumping-kwh pumping-cost-per-kwh))

                               ;; These include reduced heat or extra
                               ;; cold from pumping, because we have
                               ;; adjusted output-kwh already.
                               ::solution/supply-emissions
                               (into {}
                                     (for [e candidate/emissions-types]
                                       (let [supply-factor (get (::supply/emissions v) e 0) ;; kg/kwh
                                             emissions     (* supply-factor output-kwh)]
                                         [e (finance/emissions-value instance e emissions)])))

                               ;; However, we also need to compute the pumping emissions separately
                               ::solution/pumping-emissions
                               (into {}
                                     (for [e candidate/emissions-types]
                                       (let [supply-factor (get pumping-emissions e 0) ;; kg/kwh
                                             emissions     (* supply-factor pumping-kwh)]
                                         [e (finance/emissions-value instance e emissions)])))
                               )))
                  
                  
                  ))))

        ;; in kw -> diameter
        civil-exponent       (::document/civil-cost-exponent instance 1)
        mechanical-fixed     (::document/mechanical-cost-per-m instance 0)
        mechanical-variable  (::document/mechanical-cost-per-m2 instance 0)
        mechanical-exponent  (::document/mechanical-cost-exponent instance 1)
        
        update-edge
        (fn [e]
          (let [solution-edge (solution-edges (::candidate/id e))
                candidate-length (::path/length e)
                input-length (or (attr/attr net-graph [(:i solution-edge) (:j solution-edge)] :length) candidate-length)
                length-factor (safe-div candidate-length input-length)
                diameter-mm (* (pipes/linear-evaluate power-curve (:capacity-kw solution-edge)) 1000.0)

                civil-cost-id (::path/civil-cost-id e)

                principal
                (let [{civil-fixed ::path/fixed-cost civil-variable ::path/variable-cost}
                      (document/civil-cost-for-id instance civil-cost-id)]
                  (path/cost e
                             civil-fixed civil-variable civil-exponent
                             mechanical-fixed mechanical-variable mechanical-exponent
                             diameter-mm))

                capex-type
                (if (::path/exists e)
                  :existing-pipe-capex
                  :pipe-capex)
                
                ]

            ;; the path cost we're working out here is the 'truth'
            ;; as opposed to the linearised truth, so there will be a little error
            (assoc e
                   ::solution/length-factor length-factor
                   ::solution/included      true
                   ::solution/diameter-mm   diameter-mm
                   ::solution/capacity-kw   (:capacity-kw solution-edge)
                   ::solution/diversity     (:diversity solution-edge)
                   ::solution/pipe-capex     (finance/adjusted-value
                                              instance capex-type principal)     
                   
                   ::solution/losses-kwh    (* HOURS-PER-YEAR length-factor (:losses-kw solution-edge)))
            ))
        ]
    (if (= state :error)
      instance

      (-> instance
          (document/map-candidates update-vertex (keys solution-vertices))
          (document/map-candidates update-edge (keys solution-edges))
          (assoc ::solution/objective (:objective result-json)
                 ::solution/bounds
                 (-> result-json :solver :bounds)
                 ::solution/iterations
                 (-> result-json :solver :iterations)
                 ::solution/gap
                 (-> result-json :solver :gap)
                 ::solution/objectives
                 (-> result-json :solver :objectives)
                 )))))

(defn- mark-unreachable [instance net-graph included-candidates]
  (let [ids-in-net-graph
        (->> (graph/nodes net-graph)
             (filter #(attr/attr net-graph % :real-vertex))
             (concat
              (->> (graph/edges net-graph)
                   (mapcat #(attr/attr net-graph % :ids))))
             (set))

        ids-in-instance
        (set (map ::candidate/id included-candidates))]
    
    (document/map-candidates
     instance
     #(assoc % ::solution/unreachable true)
     (set/difference ids-in-instance ids-in-net-graph))))

(defn- make-market-decisions
  "Some people in INSTANCE may be on the special tariff called :market.
  In this case we need to work out a few things:

  - What unit rate we are going to offer them to connect to us
  - What decisions we think they should make as their next best option.
  "
  [instance]

  
  (let [market-term       (max 1 (::tariff/market-term instance 1))
        market-rate       (::tariff/market-discount-rate instance 0)
        market-stickiness (::tariff/market-stickiness instance 0)
        emissions-costs   (::document/emissions-cost instance)
        alternatives      (::document/alternatives instance {})
        insulations       (::document/insulation instance {})
        ]
    (->>
     (for [[k v] (::document/candidates instance)
           :when (= :market (::tariff/id v))]
       (let [areas        {:roof  (::candidate/roof-area v 0)
                           :wall  (::candidate/wall-area v 0)
                           :floor (::candidate/ground-area v 0)}

             counterfactual (::demand/counterfactual v)
             
             alternatives
             (cond-> (for [i (::demand/alternatives v)]
                       (get alternatives i))

               ;; TODO we are not repeating the capex for this right
               ;; nor for the other options, so it is not quite fair -
               ;; the CF will mostly win, unless we turn down DR etc.
               counterfactual
               (conj
                (assoc (get alternatives counterfactual)
                       ::supply/capex-per-kwp 0
                       ::supply/capex-per-mean-kw 0
                       ::supply/fixed-cost 0
                       )))
             
             insulations
             (for [i (::demand/insulation v)]
               (get insulations i))

             market-decision
             (market/evaluate market-term
                              market-rate
                              market-stickiness

                              (candidate/peak-demand v (document/mode instance))
                              (candidate/annual-demand v (document/mode instance))
                              
                              areas

                              emissions-costs
                              alternatives
                              insulations)]

         [k market-decision]))
     (into {}))))

(defn- paths-into [x from]
  (lazy-seq
   (cond
     (map? x)
     (mapcat
      (fn [[k v]] (paths-into v (conj from k)))
      x)
     
     (or (vector? x) (list? x))
     (apply
      concat
      (map-indexed
       (fn [i v]
         (paths-into v (conj from i)))
       x))
  
     :else
     (list [from x]))))

(defn- find-bad-numbers
  {:test #(do (= #{[:a] [:b :c]}
                 (set (find-bad-numbers {:a ##NaN
                                         :b {:c ##NaN :d 1}
                                         :e 1
                                         :f "A string"
                                         }))))}
    [data]
  (->> (paths-into data [])
       (keep (fn [[a b]]
               (when (and (number? b)
                          (not (Double/isFinite b)))
                 a)))))



(defn solve
  "Solve the INSTANCE, returning an updated instance with solution
  details in it. Probably needs running off the main thread."
  [label instance & {:keys [remove-temporary-files]}]
  
  (let [instance (document/remove-solution instance)

        ;; working-directory (util/create-temp-directory!
        ;;                    (config :solver-directory)
        ;;                    label)
        
        ;; input-file (io/file working-directory "problem.json")

        solver-command (config :solver-command)

        included-candidates (->> (::document/candidates instance)
                                 (vals)
                                 (filter candidate/is-included?)
                                 ;; we also merge in the mechanical
                                 ;; engineering cost data for every
                                 ;; path, as that makes things easier.

                                 (map #(cond-> %
                                         (candidate/is-path? %)
                                         (merge (document/civil-cost-for-id instance (::path/civil-cost-id %)))
                                         )))

        
        net-graph (simplify-topology included-candidates)
        net-graph (summarise-attributes net-graph included-candidates)

        ;; at this point we should check for some more bad things:

        ;; * components not connected to any supply vertex
        net-graph
        (let [ccs (map set (graph-alg/connected-components net-graph))
              supplies (map ::candidate/id (filter candidate/has-supply? included-candidates))

              ;; if we are offering alternative systems we don't need to do this
              
              invalid-ccs (filter (fn [cc] (not-any? cc supplies)) ccs)]
          (cond
            (empty? invalid-ccs)
            net-graph ;; do nothing if no invalid CCs

            (::document/consider-alternatives instance)
            (do (log/info "Removing unusable edges contained in disconnected components")
                (reduce (fn [g cc]
                          (-> g
                              (graph/remove-edges* (graph/edges (graph/subgraph g cc)))
                              (attr/add-attr-to-nodes :unreachable true cc)))
                        net-graph invalid-ccs))

            :else
            (do (log/info "Removing disconnected components")
                (reduce (fn [g cc] (graph/remove-nodes* g cc))
                        net-graph invalid-ccs))))
                
        ;; This is now the topology we want. Every edge may be several
        ;; input edges, and nodes can either be real ones or junctions
        ;; that are needed topologically.

        ;; Edges can have attributes :ids, which say the input paths,
        ;; and :cost which say the total pipe cost for the edge.
        ]
    ;; check whether there are actually any vertices
    (cond
      (empty? (graph/nodes net-graph))
      (-> instance
          (assoc ::solution/log "The problem is empty - you need to include some buildings and paths in the problem for the optimiser to consider"
                 ::solution/state :empty-problem
                 ::solution/message "Empty problem"
                 ::solution/runtime 0)
          (mark-unreachable net-graph included-candidates))
      
      :else
      (let [power-curve (pipes/power-curve (::document/flow-temperature instance)
                                           (::document/return-temperature instance)
                                           (::document/minimum-pipe-diameter instance 0.02)
                                           (::document/maximum-pipe-diameter instance 1.0))

            
            market (make-market-decisions instance)

            input-json (instance->json instance net-graph power-curve market)
            
            bad-numbers (find-bad-numbers input-json)
            
            zero-nans (fn [x]
                        (if (and (number? x)
                                 (Double/isNaN x))
                          0.0
                          x))
            
            input-json (postwalk zero-nans input-json)]


        (doseq [p bad-numbers]
          (log/error "Invalid numeric value at" p))
        
        ;; (with-open [writer (io/writer input-file)]
        ;;   (json/write input-json writer :escape-unicode false))


        
        (log/info "Starting solver")
        
        (let [start-time (System/currentTimeMillis)
              ;; output (sh solver-command "problem.json" "solution.json"
              ;;            :dir working-directory)

              result (net-model/run-model input-json)
              
              end-time (System/currentTimeMillis)

              ;; output-json
              ;; (try
              ;;               (with-open [r (io/reader (io/file working-directory "solution.json"))]
              ;;                 (json/read r :key-fn keyword))
              ;;               (catch Exception ex
              ;;                 {:state :error
              ;;                  :message (.getMessage ex)}))

              solved-instance
              (-> instance
                  (assoc
                   ::solution/log (str
                                   (when (seq bad-numbers)
                                     (str "WARNING: invalid inputs at: \n"
                                          (string/join "\n" (map str bad-numbers))))
                                   ;(:out output) "\n" (:err output)
                                   )
                   
                   ::solution/state (:state result)
                   ::solution/message (:message result)
                   ::solution/runtime (safe-div (- end-time start-time) 1000.0))
                  (merge-solution net-graph power-curve market result)
                  (mark-unreachable net-graph included-candidates))
              ]

          ;; (if remove-temporary-files
          ;;   (util/remove-files! working-directory)
          ;;   (do
          ;;     (spit (io/file working-directory "stdout.txt") (:out output))
          ;;     (spit (io/file working-directory "stderr.txt") (:err output))
          ;;     (spit (io/file working-directory "instance.edn") solved-instance)))

          
          solved-instance)))
    ))



(defn try-solve [label instance & {:keys [remove-temporary-files]}]
  (try
    (solve label instance :remove-temporary-files remove-temporary-files)

    (catch Throwable ex
      (util/dump-error ex "Error running network problem"
                       :type "network" :data instance)
      (-> instance
          (document/remove-solution)
          (assoc ::solution/state :uncaught-error
                 ::solution/log   (with-out-str
                                    (clojure.stacktrace/print-throwable ex)
                                    (println "---")
                                    (clojure.stacktrace/print-cause-trace ex)))))))



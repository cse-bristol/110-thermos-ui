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
            [thermos-util :refer [annual-kwh->kw]]
            [thermos-util.finance :as finance]
            [thermos-util.pipes :as pipes]
            [clojure.walk :refer [postwalk]]

            [thermos-specs.tariff :as tariff]
            [thermos-specs.measure :as measure])
  
  (:import [java.io StringWriter]))

(def HOURS-PER-YEAR 8766)

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

        net-graph (apply
                   graph/graph
                   (concat
                    (mapcat ::candidate/connections buildings)

                    (map ::path/start paths)
                    (map ::path/end paths)
                    
                    (map ::candidate/id buildings)

                    (map #(vector (::path/start %) (::path/end %)) paths)
                    (mapcat #(for [c (::candidate/connections %)] [c (::candidate/id %)]) buildings)))

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
                                (attr/add-attr (::path/start p) (::path/end p)
                                               :variable-cost (::path/variable-cost p 0))
                                (attr/add-attr (::path/start p) (::path/end p)
                                               :fixed-cost (::path/fixed-cost p 0))))
                          net-graph
                          paths)

        ;; delete all edges which do nothing
        net-graph (graph/remove-edges*
                   net-graph
                   (filter (fn [e] (= (second e) (first e)))
                           (graph/edges net-graph)))
        
        collapse-junction
        ;; this is a function to take a graph and delete a node,
        ;; preserving the identiy information on the edges. This will
        ;; later let us emit the edges into the output usefully.
        (fn [graph node]
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
              graph)))

        equal-costs
        ;; Test whether two edges have combinable cost terms.
        ;; TODO There's a bit of room for improvement - if two edges have only fixed costs the fixed costs are combinable
        (fn [net-graph v]
          (let [[e1 e2] (graph/out-edges net-graph v)
                variable-cost-1 (attr/attr net-graph e1 :variable-cost)
                variable-cost-2 (attr/attr net-graph e2 :variable-cost)
                fixed-cost-1 (attr/attr net-graph e1 :fixed-cost)
                fixed-cost-2 (attr/attr net-graph e2 :fixed-cost)]
            (and (= variable-cost-1 variable-cost-2)
                 (= fixed-cost-1 fixed-cost-2))))
        
        ;; collapse all collapsible edges until we have finished doing so.
        net-graph
        (loop [net-graph net-graph]
          (let [collapsible (->> (graph/nodes net-graph)
                                 (filter #(not (attr/attr net-graph % :real-vertex)))
                                 (filter #(= 2 (graph/out-degree net-graph %)))
                                 (filter #(equal-costs net-graph %)))]
            (if (empty? collapsible)
              net-graph
              ;; this should be OK because we are working on nodes.
              ;; removing one collapsible node does not make another
              ;; collapsible node invalid.
              (recur (reduce collapse-junction net-graph collapsible)))))

        ;; prune all spurious junctions
        net-graph
        (loop [net-graph net-graph]
          (let [spurious (->> (graph/nodes net-graph)
                              (filter #(not (attr/attr net-graph % :real-vertex)))
                              (filter #(= 1 (graph/out-degree net-graph %))))]
            (if (empty? spurious)
              net-graph
              ;; this should be OK because we are working on nodes.
              ;; removing one collapsible node does not make another
              ;; collapsible node invalid.
              (recur (graph/remove-nodes* net-graph spurious)))))
        ]
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

        total-value
        (fn [path-ids value]
          (let [costs (map #(or (value (paths %)) 0) path-ids)]
            (apply + costs)))
        
        total-requirement
        (fn [path-ids]
          (if (some (partial = :required)
                    (map (comp ::candidate/inclusion paths) path-ids))
            :required
            :optional))
        ]

    (reduce (fn [g e]
              (let [path-ids (attr/attr g e :ids)]
                (-> g
                    (attr/add-attr e :length (total-value path-ids ::path/length))
                    (attr/add-attr e :requirement (total-requirement path-ids)))))
            
            net-graph (graph/edges net-graph))))

(defn- demand-value-terms
  "Given a vertex, calculate the value terms for it. These are value, value/kwh, and value/kwp.
  They are needed separately to allow the optimiser to think about demand reduction measures."
  [instance candidate]

  (let [ignore-revenues (= :system (::document/objective instance))

        kwh (float   (::demand/kwh candidate 0))
        kwp (float   (::demand/kwp candidate (annual-kwh->kw (::demand/kwh candidate 0))))

        {standing-charge     ::tariff/standing-charge
         unit-charge         ::tariff/unit-charge
         capacity-charge     ::tariff/capacity-charge
         fixed-connection    ::tariff/fixed-connection-cost
         variable-connection ::tariff/variable-connection-cost}
        (document/tariff-for-id instance (::tariff/id candidate))

        standing-charge (if ignore-revenues 0 (or standing-charge 0))
        unit-charge     (if ignore-revenues 0 (or unit-charge 0))
        capacity-charge (if ignore-revenues 0 (or capacity-charge 0))

        fixed-connection (or fixed-connection 0)
        variable-connection (or variable-connection 0)
        ]
    {:kwh        kwh
     :kwp        kwp
     :value      (float (+ (finance/objective-value instance
                                                    :heat-revenue
                                                    standing-charge)
                           (finance/objective-value instance
                                                    :connection-capex
                                                    fixed-connection)))
     
     "value/kwh" (float (finance/objective-value instance :heat-revenue unit-charge))
     "value/kwp" (float (+ (finance/objective-value instance :heat-revenue capacity-charge)
                           (finance/objective-value instance :connection-capex variable-connection)))}))

(defn- insulation-max-area [measure candidate]
  (let [area (get candidate
                  (case (::measure/surface measure)

                    :roof  ::candidate/roof-area
                    :floor ::candidate/ground-area
                    
                    ::candidate/wall-area)
                  0)]
    (* area (::measure/maximum-area measure 0))))

(defn- insulation-max-effect [measure candidate]
  (* (::measure/maximum-effect measure 0)
     (::demand/kwh candidate 0)))

(defn- insulation-definitions [instance candidate]
  {:insulation
   (->> (when (::document/consider-insulation instance)
          (for [insulation-id (::demand/insulation candidate)
                :let [measure (get-in instance [::document/insulation insulation-id])
                      area (insulation-max-area measure candidate)
                      maximum-kwh-saved (insulation-max-effect measure candidate)]
                :when (and measure
                           (pos? area)
                           (pos? maximum-kwh-saved))]
            (let [cost-per-m2 (::measure/cost-per-m2 measure 0)
                  maximum-cost (* area cost-per-m2)
                  cost-per-kwh (/ maximum-cost maximum-kwh-saved)]
              {:id insulation-id
               :cost      (finance/objective-value instance
                                                   :insulation-capex
                                                   (::measure/fixed-cost measure 0))
               "cost/kwh" (finance/objective-value instance
                                                   :insulation-capex
                                                   cost-per-kwh)
               :maximum   maximum-kwh-saved})))
        (filter identity))})

(defn- alternative-definitions [instance candidate]
   {:alternatives
    (let [counterfactual (::demand/counterfactual candidate)
          ids (set
               (conj
                (when (::document/consider-alternatives instance)
                  (::demand/alternatives candidate))
                counterfactual))

          network-only (= :network (::document/objective instance :network))]
      (->>
       (for [id ids]
         (when-let [alternative (get-in instance [::document/alternatives id])]
           {:id id
            :cost      (if (or network-only (= counterfactual id))
                         0
                         (finance/objective-value instance :alternative-capex
                                                  (::supply/fixed-cost alternative 0)))

            ;; we pay for fuel for the counterfactual
            "cost/kwh" (if network-only 0
                           (finance/objective-value instance :alternative-opex
                                                    (::supply/cost-per-kwh alternative 0)))
            
            "cost/kwp"
            ;; we pay opex for the CF unless in network-only mode.
            (if network-only
              0
              (finance/objective-value instance :alternative-opex
                                       (::supply/opex-per-kwp alternative 0)))
            
            :emissions
            (into {}
                  (for [e candidate/emissions-types]
                    [e (float (get-in alternative [::supply/emissions e] 0))]))}))
       (filter identity)))})

(defn demand-terms [instance candidate]
  (merge
   {:required  (boolean (candidate/required? candidate))
    :count     (int     (::demand/connection-count candidate 1))}
   (demand-value-terms instance candidate)
   (insulation-definitions instance candidate)
   (alternative-definitions instance candidate)))

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
        ]
    
    {:capacity-kw (float (::supply/capacity-kwp  candidate 0))
     :cost        (float fixed-cost)
     "cost/kwh"   (float heat-cost-per-kwh)
     "cost/kwp"   (float (+ capex-per-kwp opex-per-kwp))

     :emissions   (into {}
                        (for [e candidate/emissions-types]
                          [e (float (get-in candidate [::supply/emissions e] 0))]))}))

(defn- edge-terms [instance bounds net-graph edge]
  (let [{[lower upper] :mean} bounds

        [fixed-cost variable-cost]
        (pipes/linear-cost-per-kw
         (- (::document/flow-temperature instance)
            (::document/return-temperature instance))
         (apply max lower) (apply max upper) ;; forwards vs backwards - TODO should I think about zeroes specially here?

         (::document/mechanical-cost-per-m instance 0.0)
         (::document/mechanical-cost-per-m2 instance 0.0)
         (::document/mechanical-cost-exponent instance 1.0)
         
         (or (attr/attr net-graph edge :fixed-cost) 0.0)
         (or (attr/attr net-graph edge :variable-cost) 0.0)
         (::document/civil-cost-exponent instance 1.0))
        ]
    {:i (first edge) :j (second edge)
     :length    (float (or (attr/attr net-graph edge :length) 0))
     "cost/m"   (float (finance/objective-value instance :pipe-capex fixed-cost))
     "cost/kwm" (float (finance/objective-value instance :pipe-capex variable-cost))
     :bounds bounds
     :required (boolean (attr/attr net-graph edge :required))}))

(defn- instance->json [instance net-graph]
  (let [candidates     (::document/candidates instance)

        demand-ids (map ::candidate/id (filter candidate/has-demand? (vals candidates)))
        supply-ids (map ::candidate/id (filter candidate/has-supply? (vals candidates)))

        edge-bounds (do
                      (log/info "Computing flow bounds...")
                      (bounds/edge-bounds
                       net-graph
                       :max-kwp (::document/maximum-pipe-kwp instance)
                       :capacity (comp #(::supply/capacity-kwp % 0) candidates)
                       :demand (comp annual-kwh->kw #(::demand/kwh % 0) candidates)
                       :peak-demand (comp #(::demand/kwp % 0) candidates)
                       :size (comp #(::connection-count % 1) candidates)))

        _ (log/info "Computed flow bounds")
        
        global-factors (::demand/emissions instance)]
    
    {:time-limit  (float (::document/maximum-runtime instance 1.0))
     :mip-gap     (float (::document/mip-gap instance 0.05))
     :pipe-losses
     (let [losses (pipes/heat-loss-curve
                   (::document/flow-temperature instance)
                   (::document/return-temperature instance)
                   (::document/ground-temperature instance))]
       {:kwp  (map (comp float first) losses)
        "w/m" (map (comp float second) losses)})

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
                         {:limit (float (get-in instance [::document/emissions-limit :value e]))}))]))

     :vertices
     (for [vertex (graph/nodes net-graph)
           :let [candidate (candidates vertex)]
           :when (or (candidate/has-demand? candidate)
                     (candidate/has-supply? candidate))]
       (cond-> {:id vertex}
         (candidate/has-demand? candidate)
         (assoc :demand (demand-terms instance candidate))

         (candidate/has-supply? candidate)
         (assoc :supply (supply-terms instance candidate))))
     
     :edges
     (for [edge (->> (graph/edges net-graph)
                     (map (comp vec sort))
                     (set))]
       (edge-terms instance (get edge-bounds edge) net-graph edge))}))

(defn- index-by [f vs]
  (reduce #(assoc %1 (f %2) %2) {} vs))

(defn- output-insulation [instance candidate id kwh]
  (when (pos? kwh)
    (when-let [insulation (document/insulation-for-id instance id)]
      (let [fixed-cost  (::measure/fixed-cost insulation 0)
            cost-per-m2 (::measure/cost-per-m2 insulation 0)
            max-area    (insulation-max-area insulation candidate)
            max-effect  (insulation-max-effect insulation candidate)
            proportion-done (/ kwh max-effect)
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
                 kwp (::demand/kwp candidate)
                 kwh (::demand/kwh candidate)
                 opex (* opex-per-kwp kwp)
                 fuel (* cost-per-kwh kwh)]
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
  (assoc candidate
         ::solution/alternative
         (if (= (::demand/counterfactual candidate)
                (::supply/id alternative))
           (assoc (::solution/counterfactual candidate)
                  :counterfactual true)
           (let [kwh (::solution/kwh candidate)
                 kwp (::demand/kwp candidate)
                 
                 fixed-cost (::supply/fixed-cost alternative 0)
                 cost-per-kwp (::supply/cost-per-kwp alternative 0)
                 
                 opex-per-kwh (::supply/cost-per-kwh alternative 0)
                 opex-per-kwp (::supply/opex-per-kwp alternative 0)
                 
                 capex (+ fixed-cost (* cost-per-kwp kwp))
                 opex (* opex-per-kwp kwp)
                 fuel (* opex-per-kwh kwh)]
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
              ::supply/name (::supply/name alternative)}))))


(defn- merge-solution [instance net-graph result-json]
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
                tariff          (document/tariff-for-id instance (::tariff/id v))
                
                insulation           (for [[id kwh] (:insulation solution-vertex)]
                                       (output-insulation instance v id kwh))

                total-insulation-kwh (reduce + 0 (map :kwh insulation))
                effective-demand     (- (::demand/kwh v) total-insulation-kwh)

                alternative          (document/alternative-for-id
                                      instance
                                      (:alternative solution-vertex))

                heat-revenue
                (finance/adjusted-value
                 instance
                 :heat-revenue
                 (tariff/annual-heat-revenue
                  tariff
                  effective-demand
                  (::demand/kwp v)))

                connection-cost
                (finance/adjusted-value
                 instance
                 :connection-capex
                 (tariff/connection-cost
                  tariff
                  effective-demand
                  (::demand/kwp v)))

                [supply-capex
                 supply-opex
                 supply-heat-cost]
                (when-let [supply-capacity (:capacity-kw solution-vertex)]
                  [(finance/adjusted-value
                    instance
                    :supply-capex
                    (supply/principal v (:capacity-kw solution-vertex)))

                   (finance/adjusted-value
                    instance
                    :supply-opex
                    (supply/opex v (:capacity-kw solution-vertex)))

                   (finance/adjusted-value
                    instance
                    :supply-heat
                    (supply/heat-cost v (:output-kwh solution-vertex)))
                   ])
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

                  ;; supply facts
                  (:capacity-kw solution-vertex)
                  (assoc ::solution/capacity-kw   (:capacity-kw solution-vertex)
                         ::solution/diversity     (:diversity solution-vertex)
                         ::solution/output-kwh    (:output-kwh solution-vertex)
                         ::solution/supply-capex  supply-capex 
                         ::solution/supply-opex   supply-opex
                         ::solution/heat-cost     supply-heat-cost
                         ::solution/supply-emissions
                         (into {}
                               (for [e candidate/emissions-types]
                                 (let [supply-factor      (get (::supply/emissions v) e 0) ;; kg/kwh
                                       emissions (* supply-factor (:output-kwh solution-vertex))]
                                   [e (finance/emissions-value instance e emissions)]))))
                  ))))

        ;; in kw -> diameter
        power-curve   (pipes/power-curve (- (::document/flow-temperature instance)
                                            (::document/return-temperature instance)))
        civil-exponent       (::document/civil-cost-exponent instance 1)
        mechanical-fixed     (::document/mechanical-cost-per-m instance 0)
        mechanical-variable  (::document/mechanical-cost-per-m2 instance 0)
        mechanical-exponent  (::document/mechanical-cost-exponent instance 1)
        
        update-edge
        (fn [e]
          (let [solution-edge (solution-edges (::candidate/id e))
                candidate-length (::path/length e)
                input-length (or (attr/attr net-graph [(:i solution-edge) (:j solution-edge)] :length) candidate-length)
                length-factor (if (zero? candidate-length) 0 (/ candidate-length input-length))
                diameter-mm (* (pipes/linear-evaluate power-curve (:capacity-kw solution-edge)) 1000.0)

                {civil-fixed ::path/fixed-cost civil-variable ::path/variable-cost}
                (document/civil-cost-for-id instance (::path/civil-cost-id e))

                principal
                (path/cost e
                           civil-fixed civil-variable civil-exponent
                           mechanical-fixed mechanical-variable mechanical-exponent
                           diameter-mm)
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
                                             instance :pipe-capex principal)     
                   
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

(defn- mark-unreachable [instance net-graph]
  (let [ids-in-net-graph
        (->> (graph/nodes net-graph)
             (filter #(attr/attr net-graph % :real-vertex))
             (concat
              (->> (graph/edges net-graph)
                   (mapcat #(attr/attr net-graph % :ids))))
             (set))

        ids-in-instance (set (keys (::document/candidates instance)))]
    
    (document/map-candidates
     instance
     #(assoc % ::solution/unreachable true)
     (set/difference ids-in-instance ids-in-net-graph))))

(defn solve
  "Solve the INSTANCE, returning an updated instance with solution
  details in it. Probably needs running off the main thread."
  [label instance]
  
  (let [instance (document/remove-solution instance)

        working-directory (util/create-temp-directory!
                           (config :solver-directory)
                           label)
        
        input-file (io/file working-directory "problem.json")

        solver-command (config :solver-command)

        included-candidates (->> (::document/candidates instance)
                                 (vals)
                                 (filter candidate/is-included?)
                                 ;; we also merge in the mechanical
                                 ;; engineering cost data for every
                                 ;; path, as that makes things easier.
                                 (map #(cond-> %
                                         (candidate/is-path? %)
                                         (merge (document/civil-cost-for-id instance (::path/civil-cost-id %))))))
        
        net-graph (simplify-topology included-candidates)
        net-graph (summarise-attributes net-graph included-candidates)

        ;; at this point we should check for some more bad things:

        ;; * components not connected to any supply vertex
        ccs (map set (graph-alg/connected-components net-graph))
        supplies (map ::candidate/id (filter candidate/has-supply? included-candidates))

        invalid-ccs (filter (fn [cc] (not-any? cc supplies)) ccs)

        _ (log/info "removing" (count invalid-ccs) "un-suppliable components"
                    "containing" (reduce + 0 (map count invalid-ccs)) "vertices")
        
        net-graph (reduce (fn [g cc] (graph/remove-nodes* g cc))
                          net-graph invalid-ccs)
                
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
          (mark-unreachable net-graph))
      
      :else
      (let [input-json (postwalk identity (instance->json instance net-graph))]
        (log/info "Output scenario to" input-file)
        (with-open [writer (io/writer input-file)]
          (json/write input-json writer :escape-unicode false))
        
        (log/info "Starting solver")
        ;; invoke the solver
        (let [start-time (System/currentTimeMillis)
              output (sh solver-command "problem.json" "solution.json"
                         :dir working-directory)
              
              end-time (System/currentTimeMillis)

              _ (log/info "Solver ran in" (- end-time start-time) "ms")

              output-json (try
                            (with-open [r (io/reader (io/file working-directory "solution.json"))]
                              (json/read r :key-fn keyword))
                            (catch Exception ex
                              {:state :error
                               :message (.getMessage ex)}))

              solved-instance
              (-> instance
                  (assoc
                   ::solution/log (str (:out output) "\n" (:err output))
                   ::solution/state (:state output-json)
                   ::solution/message (:message output-json)
                   ::solution/runtime (/ (- end-time start-time) 1000.0))
                  (merge-solution net-graph output-json)
                  (mark-unreachable net-graph))
              ]
          (spit (io/file working-directory "stdout.txt") (:out output))
          (spit (io/file working-directory "stderr.txt") (:err output))
          (spit (io/file working-directory "instance.edn") solved-instance)
          solved-instance)))
    
    ;; write the scenario down
    ))

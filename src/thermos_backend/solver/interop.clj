;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.solver.interop
  "This is a translation layer between the UI representation of a problem, and the network model format.

  The input to the network model is described in thermos.opt.net.specs.

  The main distinctions made here are:

  - The network model doesn't care about NPV or financing; it just
    trades off interchangeable costs. NPV and loans are applied here
    to produce single figures for the network model.

    The user interface does care about NPV and financing, so the NPV
    figures are recomputed on the way out for display purposes and stuck
    back onto the UI representation.

  - A certain amount of graph simplification is done in here, to
    shrink the problem before the network model sees it

  - Pipe costs are understood in the UI to be taken from a table.
    Power / diameter relationship is understood in terms of the medium

    The network model doesn't know this, it knows fixed cost and cost/kw

    This translation layer approximates pipe costs down to fixed + variable.
    Then it rounds up the decisions that have been made to the next pipe in the table.

  - In the UI, individual / alternative systems can have a 'hot water tank'
    which 'reduces the peak' for the individual system. This is not expressed in the
    network model, but is applied in here by transforming the /costs/ which the
    model optimises on.
  "
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
            [thermos.opt.net.diversity :as net-diversity]
            [thermos.opt.net.bounds :as net-model-bounds]
            [thermos-backend.solver.logcap :as logcap]
            )
  
  (:import [java.io StringWriter]))

(def HOURS-PER-YEAR 8766)

(defn create-graph [buildings paths]
  (let [graph (apply
               graph/graph
               (concat
                (mapcat ::candidate/connections buildings)

                (map ::path/start paths)
                (map ::path/end paths)
                
                (map ::candidate/id buildings)

                (map #(vector (::path/start %) (::path/end %)) paths)
                (mapcat #(for [c (::candidate/connections %)] [c (::candidate/id %)]) buildings)))
        graph (reduce
               (fn [g b]
                 (-> g
                     (attr/add-attr (::candidate/id b) :real-vertex true)
                     (cond->
                         
                       (candidate/has-supply? b)
                       (attr/add-attr (::candidate/id b) :supply-vertex true)
                       )))
               graph
               buildings)
        graph (reduce
               (fn [g p] (attr/add-attr g (::path/start p) (::path/end p) :ids #{(::candidate/id p)}))
               graph
               paths)

        graph (graph/remove-edges*
               graph
               (filter (fn [e] (= (second e) (first e)))
                       (graph/edges graph)))
        ]
    graph
    ))

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
  [net-graph]

  (let [;; TODO : this could be improved - what we really want is all the 2-cuts
        ;; in the graph which isolate a subgraph containing no real-vertex
        collapsible? (fn [net-graph node]
                       (and (not (attr/attr net-graph node :real-vertex))
                            (= 2 (graph/out-degree net-graph node))))

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

        ;; Prune all dangling junctions. This removes all the frills on the network.
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
    
    net-graph))

(defn- clean-disconnected-components
  "Anything in the network graph which is not reachable from a supply
  shouldn't be considered for heat networking. However sometimes we
  are considering other heating systems, in which case we only want to
  delete the edges.

  This builds a directed copy of net-graph which respects the rule that
  a building may not be supplied with heat through another building, so

  S ---> D1 <--- D2

  means D2 is effectively disconnected.

  This is why we don't directly use connected-components below but instead
  DF traverse from each supply.
  
  Returns a tuple: [modified graph, candidate IDs removed]"
  [net-graph only-remove-edges]
  (let [directed-net-graph
        (reduce
         (fn [g [i j]]
           (let [i-supply (attr/attr net-graph i :supply-vertex)
                 j-supply (attr/attr net-graph j :supply-vertex)
                 i-real   (attr/attr net-graph i :real-vertex)
                 j-real   (attr/attr net-graph j :real-vertex)]
             (cond-> g
               ;; if i is real and not a supply, don't add i-j
               ;; if j is real and not a supply, don't add j-i

               (not (and i-real (not i-supply)))
               (graph/add-edges [i j])

               (not (and j-real (not j-supply)))
               (graph/add-edges [j i]))))
         
         (graph/digraph)
         
         (into #{} (graph/edges net-graph)))

        supplies (filter #(attr/attr net-graph % :supply-vertex)
                         (graph/nodes net-graph))
        
        supplied-vertices
        (into
         #{}
         (mapcat
          #(graph-alg/bf-traverse directed-net-graph %)
          supplies))

        unsupplied-vertices
        (set/difference
         (set (graph/nodes net-graph))
         supplied-vertices)        
        ]
    (if (empty? unsupplied-vertices)
      [net-graph []]

      (let [dead-verts unsupplied-vertices
            dead-edges (graph/edges (graph/subgraph net-graph dead-verts))
            
            graph-without-dead-stuff
            (-> net-graph
             (graph/remove-edges* dead-edges)
             (cond->
                 only-remove-edges
               (attr/add-attr-to-nodes :unreachable true dead-verts)

               (not only-remove-edges)
               (graph/remove-nodes* dead-verts)))
            ]

        [graph-without-dead-stuff
         (into
          (set (mapcat #(attr/attr net-graph % :ids) dead-edges))
          (filter #(attr/attr net-graph % :real-vertex) dead-verts))]))))

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

        length-by-civils
        (fn [path-ids]
          (reduce
           (fn [acc id]
             (let [path     (paths id)
                   length   (::path/length path 0)
                   civil-id (::path/civil-cost-id path)
                   ]
               (assoc acc civil-id (+ length (get acc civil-id 0)))))
           {}
           path-ids))
        
        total-value
        (fn [path-ids value]
          (let [costs (map #(or (value (paths %)) 0) path-ids)]
            (apply + costs)))
        
        total-requirement
        (fn [path-ids]
          (let [requirements (map (comp ::candidate/inclusion paths) path-ids)
                result       (if (some #{:required} requirements)
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
                      (attr/add-attr e :civil-costs (length-by-civils path-ids))
                      ))))
            
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

        external-connection (::tariff/external-connection-cost candidate)
        
        fixed-connection    (if external-connection external-connection
                                (or fixed-connection 0))
        variable-connection (if external-connection 0
                              (or variable-connection 0))
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

(defn- insulation-max-area
  "What's the most area a given insulation can apply to on a candidate"
  [measure candidate]
  (let [area (get candidate
                  (case (::measure/surface measure)

                    :roof  ::candidate/roof-area
                    :floor ::candidate/ground-area
                    
                    ::candidate/wall-area)
                  0)]
    (* area (::measure/maximum-area measure 0))))

(defn- insulation-max-effect
  "By how much can insulation reduce a demand?"
  [measure base-demand]
  (* (::measure/maximum-effect measure 0) base-demand))

(defn- insulation-definitions
  "Output for the network model a description of all the insulation options
  for candidate."
  [instance candidate]
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

(defn- alternative-definitions
  "Output for the network model a description of all the alternative
  systems for candidate.

  Responsible for distinguishing the counterfactual choice, by
  changing its capital cost and for transferring cost/kwp onto
  cost/kwh for systems which have the hot water tank behaviour.
  "
  [instance candidate]
   {:alternatives
    (let [counterfactual (::demand/counterfactual candidate)
          ids (-> (cond-> ()
                    (::document/consider-alternatives instance)
                    (concat (::demand/alternatives candidate))
                    (not (nil? counterfactual))
                    (conj counterfactual))
                  (set))]
      (for [id ids
            :let [alternative (get-in instance [::document/alternatives id])]
            :when alternative]
        (let [capex-type (if (= counterfactual id)
                           :counterfactual-capex
                           :alternative-capex)

              opex%kwp (finance/objective-value
                        instance :alternative-opex (::supply/opex-per-kwp alternative 0))

              capex%kwp (finance/objective-value
                         instance capex-type (::supply/capex-per-kwp alternative 0))
              
              cost%kwp  (cond-> (+ opex%kwp capex%kwp)
                          ;; supply/remove-diversity is an ugly thing
                          ;; which we do not tell the optimiser.
                          (::supply/remove-diversity alternative)
                          (/ (diversity (::demand/connection-count candidate 1))))
              
              
              changes-peak (::supply/kwp-per-mean-kw alternative)
              ;; if changes-peak we need to insert cost/kwp into the cost/kwh

              capex%kwh (annual-kwh->kw ;; this converts our cost/kw figures into cost/kwh
                         (+ (::supply/capex-per-mean-kw alternative 0)
                            (* (or changes-peak 0) cost%kwp)))

              ;; zero cost%kwp if the measure changes the peak
              ;; we care about this here so that the network model doesn't have to.
              cost%kwp (if changes-peak 0 (+ opex%kwp capex%kwp))
              
              cost%kwh
              (+
               (finance/objective-value instance capex-type capex%kwh)
               (finance/objective-value instance :alternative-opex
                                        (::supply/cost-per-kwh alternative 0)))]
          
          {:id id
           :cost      (+ (finance/objective-value
                          instance capex-type
                          (::supply/fixed-cost alternative 0))
                         
                         (finance/objective-value
                          instance capex-type
                          (* (::supply/capex-per-connection alternative 0)
                             (::demand/connection-count candidate 1)))
                         
                         (finance/objective-value
                          instance :alternative-opex
                          (+ (::supply/opex-fixed alternative 0)
                             (* (::supply/opex-per-connection alternative 0)
                                (::demand/connection-count candidate 1)))))

           :cost%kwh cost%kwh
           :cost%kwp cost%kwp
           
           :emissions
           (into {}
                 (for [e candidate/emissions-types]
                   [e (float (get-in alternative [::supply/emissions e] 0))]))})))})

(defn demand-terms
  "Output for the network model all the demand-related facts about this candidate"
  [instance candidate market]
  (merge
   {:required    (boolean (candidate/required? candidate))
    :off-network (boolean (candidate/only-individual? candidate))
    :count     (int     (max 1 (::demand/connection-count candidate 1)))}
   (demand-value-terms instance candidate market)

   (when (::demand/group candidate) {:group (::demand/group candidate)})

   (when-let  [infill-groups (seq (::demand/infill-groups candidate))]
     {:infill-connection-targets (set infill-groups)})
   
   ;; we only offer insulation & alternatives to the optimiser in system mode.
   (when (= :system (::document/objective instance :network))
     (insulation-definitions instance candidate))
   (when (= :system (::document/objective instance :network))
     (alternative-definitions instance candidate))))

(defn- supply-terms
  "Output for the network model all the supply-related facts about this candidate"
  [instance candidate]
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

     :exclusive-groups (::supply/exclusive-groups candidate)
     
     :emissions   emissions-factors}))

(defn- edge-terms
  "Output for the network model the parameters for this edge.

  This function is responsible for linear-approximating the civil +
  pipe costs for this edge based on the upper & lower heat flow bounds
  in bounds.
  "
  [instance
   bounds
   pipe-curves
   net-graph edge]
  (let [length (or (attr/attr net-graph edge :length) 0)

        [fixed-cost variable-cost]
        (if (zero? length) [0 0] ;; zero length path has no cost, and is probably a virtual connecty thing
            (let [forward-bounds (get bounds edge)
                  reverse-bounds (get bounds [(second edge) (first edge)])

                  lf (:diverse-peak-min forward-bounds)
                  uf (:diverse-peak-max forward-bounds)

                  lb (:diverse-peak-min reverse-bounds)
                  ub (:diverse-peak-max reverse-bounds)
                  ]
              (pipes/linear-cost
               pipe-curves

               (attr/attr net-graph edge :civil-costs)
               length ;; don't worry, this length is normalized out
                      ;; inside call to linear-cost - the result is
                      ;; per meter
               
               (if (zero? (min lf lb))
                 (max lf lb)
                 (min lf lb))
               
               (max uf ub)
               )))

        cost-type (if (attr/attr net-graph edge :exists)
                    :existing-pipe-capex
                    :pipe-capex)]

    (when (and (seq (attr/attr net-graph edge :ids))
               (not (attr/attr net-graph edge :length)))
      (log/error "Edge" edge "which maps to real edges"
                 (attr/attr net-graph edge :ids)
                 "has no recorded length"))

    {:i        (first edge) :j (second edge)
     :length   length
     :max-capacity%kwp (float
                        (if-let [max-dia (attr/attr net-graph edge :max-dia)]
                          (let [max-dia (* 1000 max-dia)]
                            (pipes/dia->kw pipe-curves max-dia))
                          (pipes/max-kw pipe-curves)))
     
     :cost%m   (float (finance/objective-value instance cost-type fixed-cost))
     :cost%kwm (float (finance/objective-value instance cost-type variable-cost))
     :required (boolean (attr/attr net-graph edge :required))}))

(defn- instance->model-input
  "Convert a problem instance into input for the network optimisation model."
  [instance net-graph pipe-curves market]
  (let [candidates     (::document/candidates instance)

        mode (document/mode instance)

        first-stage
        {:time-limit      (float (::document/maximum-runtime instance 1.0))
         :mip-gap         (float (::document/mip-gap instance 0.05))
         :param-gap       (float (::document/param-gap instance 0.0))
         :iteration-limit (int   (::document/maximum-iterations instance 1000))
         :supply-limit    (when-let [l (::document/maximum-supply-sites instance)] (int l))
         :should-be-feasible (boolean (::document/should-be-feasible instance false))
         :infill-connection-targets (::document/infill-targets instance)
         
         :pipe-losses
         (let [losses (pipes/heat-loss-curve pipe-curves)]
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
         (for [edge (->> (graph/edges net-graph)
                         (map (comp vec sort))
                         (set))]
           {:i (first edge) :j (second edge)})
         }

        bounds
        (net-model-bounds/compute-bounds first-stage)
        ]

    (log/info "Linearising costs")
    (assoc first-stage
           :edges
           (for [edge (->> (graph/edges net-graph)
                           (map (comp vec sort))
                           (set))]
             (edge-terms instance bounds pipe-curves net-graph edge))
           :bounds bounds)
    ))

(defn- index-by [f vs]
  (reduce #(assoc %1 (f %2) %2) {} vs))

(defn- output-insulation
  "Given a decision from the optimisation model that `kwh` of insulation with `id`
  should go onto `candidate`, output information for the UI describing the decision"
  [instance candidate id kwh]
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
         {::measure/name (::measure/name insulation)
          ::measure/id   id
          :kwh kwh
          :area area-done
          :proportion proportion-done}
         (finance/adjusted-value instance :insulation-capex cost))))))

(defn- alternative-adjusted-peak
  "Some alternatives 'adjust peak demand'; this is done by telling the optimiser
  their peak-related costs are kwh-related costs instead.

  For these we need to determine the 'adjusted peak' to use when displaying to the UI."
  [kwh raw-kwp alternative]
  (if-let [peak-per-base (::supply/kwp-per-mean-kw alternative)]
    (* peak-per-base (annual-kwh->kw kwh))
    raw-kwp))

(let [diversity (net-diversity/diversity-factor {})]
  (defn- evaluate-alternative [alternative kwh kwp n
                               capex-type]
    (let [n     (::demand/connection-count candidate 1)
          kwp (cond-> (alternative-adjusted-peak kwh kwp alternative)
                (::supply/remove-diversity alternative)
                (/ (diversity n)))
          
          capex (supply/principal alternative kwp kwh n)
          opex  (supply/opex alternative kwp n)
          fuel  (supply/heat-cost alternative kwh)]
      {:capex (finance/adjusted-value instance capex-type capex)
       :opex (finance/adjusted-value instance :alternative-opex opex)
       :heat-cost (finance/adjusted-value instance :alternative-opex fuel)
       :emissions
       (into {}
             (for [e candidate/emissions-types]
               (let [alternative-factor (get (::supply/emissions alternative) e 0)
                     emissions (* alternative-factor kwh)]
                 [e (finance/emissions-value instance e emissions)])))
       ::supply/id (::supply/id alternative)
       ::supply/name (::supply/name alternative)})))

(defn- output-counterfactual
  "Describe the counterfactual heating system for the UI."
  [candidate instance]
  (assoc candidate
         ::solution/counterfactual
         (if-let [alternative (document/alternative-for-id instance (::demand/counterfactual candidate))]
           (evaluate-alternative
            alternative
            (candidate/annual-demand candidate (document/mode instance))
            (candidate/peak-demand candidate (document/mode instance))
            :counterfactual-capex))))

(defn- output-alternative
  "Output information for the UI describing the choice the optimiser has
  made to use the given alternative for `instance`. If it is the
  counterfactual then costs are different."
  [candidate instance alternative]
  (let [is-counterfactual (= (::demand/counterfactual candidate)
                             (::supply/id alternative))]
    (assoc candidate
           ::solution/alternative
           (assoc 

            (if is-counterfactual
              (::solution/counterfactual candidate)
              (evaluate-alternative
               alternative
               (::solution/kwh candidate)
               (candidate/peak-demand candidate (document/mode instance))
               :alternative-capex))
            
            :counterfactual is-counterfactual))))

(defn- merge-solution
  "Combine the optimisation result back with `instance` for display to the user."
  [instance net-graph pipe-curves market result-json]
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

        copy-market-rate
        (fn [v]
          (cond-> v
            (= :market (::tariff/id v))
            (assoc ::solution/market-rate
                   (::tariff/unit-charge (market (::candidate/id v))))))
        
        update-vertex
        (fn [v]
          (let [solution-vertex (solution-vertices (::candidate/id v))
                tariff-id       (::tariff/id v)
                tariff          (if (= :market tariff-id)
                                  (market (::candidate/id v))
                                  (document/tariff-for-id instance tariff-id))

                connection-cost      (document/connection-cost-for-id instance (::tariff/cc-id v))
                
                insulation           (for [[id kwh] (:insulation solution-vertex)
                                           :when (> (Math/abs kwh) 1.0)]
                                       (output-insulation instance v id kwh))

                total-insulation-kwh (reduce + 0 (keep :kwh insulation))
                effective-demand     (- (candidate/annual-demand v (document/mode instance))
                                        total-insulation-kwh)

                alternative          (document/alternative-for-id
                                      instance
                                      (:alternative solution-vertex))

                effective-peak       (candidate/peak-demand v (document/mode instance))
                effective-peak       (alternative-adjusted-peak effective-demand effective-peak
                                                                alternative)
                
                heat-revenue
                (finance/adjusted-value
                 instance
                 :heat-revenue
                 (tariff/annual-heat-revenue tariff effective-demand effective-peak))


                external-connection-cost (::tariff/external-connection-cost v)
                
                connection-cost
                (finance/adjusted-value
                 instance
                 :connection-capex
                 (if external-connection-cost
                   external-connection-cost
                   (tariff/connection-cost connection-cost effective-demand effective-peak)))
                ]
            (-> v
                (assoc ::solution/included true
                       ::solution/kwh effective-demand
                       ::solution/kwp effective-peak)
                
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
        
        update-edge
        (fn [e]
          (let [solution-edge (solution-edges (::candidate/id e))
                candidate-length (::path/length e)
                input-length (or
                              (attr/attr net-graph [(:i solution-edge) (:j solution-edge)] :length)
                              candidate-length)
                length-factor (safe-div candidate-length input-length)
                diameter-mm   (if (and (::path/exists e)
                                       (::path/maximum-diameter e))
                                (* 1000.0 (::path/maximum-diameter e))
                                (pipes/solved-diameter pipe-curves
                                                       (:capacity-kw solution-edge)))

                max-capacity  (pipes/dia->kw pipe-curves diameter-mm)
                
                civil-cost-id (::path/civil-cost-id e)

                principal     (* candidate-length
                                 (pipes/solved-principal
                                  pipe-curves
                                  civil-cost-id
                                  (:capacity-kw solution-edge)))

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
                   ::solution/max-capacity-kw max-capacity
                   ::solution/diversity     (:diversity solution-edge)
                   ::solution/pipe-capex    (finance/adjusted-value
                                             instance capex-type principal)     
                   
                   ::solution/losses-kwh    (* HOURS-PER-YEAR length-factor (:losses-kw solution-edge)))
            ))
        ]
    (if (= state :error)
      instance

      (-> instance
          (document/map-candidates copy-market-rate)
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

(defn- mark-unreachable [instance net-graph included-candidates disconnected-candidate-ids]
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
     (fn [c]
       (assoc c ::solution/unreachable
              (if (contains? disconnected-candidate-ids (::candidate/id c))
                :disconnected :peripheral)))
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

(defn- log-empty-problem [instance]
  (assoc instance ::solution/log "The problem is empty - you need to include some buildings and paths in the problem for the optimiser to consider"
         ::solution/state :empty-problem
         ::solution/message "Empty problem"
         ::solution/runtime 0))

(defn solve
  "Solve the INSTANCE, returning an updated instance with solution
  details in it. Probably needs running off the main thread."
  [instance]
  
  (let [instance (document/remove-solution instance)

        included-candidates (->> (::document/candidates instance)
                                 (vals)
                                 (filter candidate/is-included?))

        net-graph  (let [{paths :path buildings :building}
                         (group-by ::candidate/type included-candidates)]
                     (create-graph buildings paths))

        _ (log/info "Basic graph has"
                    (count (graph/nodes net-graph))
                    "nodes,"
                    (count (graph/edges net-graph)) "edges")

        [net-graph disconnected-cand-ids]
        (clean-disconnected-components net-graph (::document/consider-alternatives instance))

        _ (log/info "Trimmed graph has"
                    (count (graph/nodes net-graph))
                    "nodes,"
                    (count (graph/edges net-graph)) "edges")
        
        net-graph (simplify-topology net-graph)

        _ (log/info "Simplified graph has"
                    (count (graph/nodes net-graph))
                    "nodes,"
                    (count (graph/edges net-graph)) "edges")
        
        net-graph (summarise-attributes net-graph included-candidates)
        ]
    
    ;; net-graph is now the topology we want. Every edge may be several
    ;; input edges, and nodes can either be real ones or junctions
    ;; that are needed topologically.

    ;; All dangling edges have been removed entirely.

    ;; Edges can have attributes :ids, which say the input paths,
    ;; and :cost which say the total pipe cost for the edge.
    
    ;; check whether there are actually any vertices
    (cond
      (empty? (graph/nodes net-graph))
      (-> (log-empty-problem instance)
          (mark-unreachable net-graph included-candidates disconnected-cand-ids))
      
      :else
      (let [pipe-curves (pipes/curves instance)
            
            market (make-market-decisions instance)

            model-input (instance->model-input instance net-graph pipe-curves market)
            
            bad-numbers (find-bad-numbers model-input)
            
            zero-nans (fn [x] (if (and (number? x) (Double/isNaN x)) 0.0 x))
            
            model-input (postwalk zero-nans model-input)]

        (doseq [p bad-numbers] (log/error "Invalid numeric value at" p))
        (log/info "Starting solver")
        
        (let [start-time (System/currentTimeMillis)
              result (if (seq (:vertices model-input))
                       (net-model/run-model model-input)
                       ::empty)
              
              end-time (System/currentTimeMillis)

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
                   ::solution/error (:error result)
                   ::solution/message (:message result)
                   ::solution/runtime (safe-div (- end-time start-time) 1000.0))
                  (cond->
                    (not= ::empty result)
                    (merge-solution net-graph pipe-curves market result)
                    
                    (= ::empty result)
                    (log-empty-problem net-graph included-candidates disconnected-cand-ids))
                  
                  (mark-unreachable net-graph included-candidates disconnected-cand-ids))
              ]
          
          solved-instance)))
    ))

(defn try-solve [instance progress]
  (let [log-writer (java.io.StringWriter.)]
    (try
      (progress :message "Start network problem")
      (let [solution (logcap/with-log-messages
                       (fn [^String msg]
                         (.write log-writer msg)
                         (.append log-writer \newline)
                         (progress :message (.toString log-writer)))
                       (solve instance))]
        (assoc solution ::solution/log (.toString log-writer)))

      (catch InterruptedException ex (throw ex))
      (catch Throwable ex
        (util/dump-error ex "Error running network problem"
                         :type "network" :data instance)
        (-> instance
            (document/remove-solution)
            (assoc ::solution/state :uncaught-error
                   ::solution/log
                   (with-out-str
                     (println (.toString log-writer))
                     (println "---")
                     (clojure.stacktrace/print-throwable ex)
                     (println "---")
                     (clojure.stacktrace/print-cause-trace ex))))))))



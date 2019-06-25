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
            [clojure.walk :refer [postwalk]]

            [thermos-specs.tariff :as tariff])
  
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
                                (attr/add-attr (::path/start p) (::path/end p)
                                               :ids #{(::candidate/id p)})
                                (attr/add-attr (::path/start p) (::path/end p)
                                               :variable-cost (::path/cost-per-m2 p 0))
                                (attr/add-attr (::path/start p) (::path/end p)
                                               :fixed-cost (::path/cost-per-m p 0))))
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
    
    {:time-limit (float (::document/maximum-runtime instance 1.0))
     :mip-gap    (float (::document/mip-gap instance 0.05))

     :flow-temperature   (float (::document/flow-temperature instance 90.0))
     :return-temperature (float (::document/return-temperature instance 60.0))
     :ground-temperature (float (::document/ground-temperature instance 8.0))

     :mechanical-fixed-cost        (float (::document/mechanical-cost-per-m instance 50.0))
     :mechanical-variable-cost     (float (::document/mechanical-cost-per-m2 instance 700.0))
     :mechanical-variable-exponent (float (::document/mechanical-cost-exponent instance 1.3))
     :civil-variable-exponent      (float (::document/civil-cost-exponent instance 1.1))
     
     :finance
     {:loan-term  (int (::document/loan-term instance))
      :loan-rate  (float (::document/loan-rate instance))
      :npv-term   (int (::document/npv-term instance))
      :npv-rate   (float (::document/npv-rate instance))}

     :emissions
     (into {}
           (for [e candidate/emissions-types]
             [e (merge {:cost    (float (get-in instance [::document/emissions-cost e] 0))}
                       (when     (get-in instance [::document/emissions-limit e :enabled])
                         {:limit (float (get-in instance [::document/emissions-limit e :value]))}))]))

     :vertices
     (for [vertex (graph/nodes net-graph)
           :let [candidate (candidates vertex)]
           :when (or (candidate/has-demand? candidate)
                     (candidate/has-supply? candidate))]
       (cond-> {:id vertex}
         (candidate/has-demand? candidate)
         (assoc :demand
                (let [tariff (document/tariff-for-id instance (::tariff/id candidate))]
                  {:kw        (float   (annual-kwh->kw (::demand/kwh candidate 0)))
                   :kwp       (float   (::demand/kwp candidate (annual-kwh->kw (::demand/kwh candidate 0))))
                   :required  (boolean (candidate/required? candidate))
                   :count     (int     (::demand/connection-count candidate 1))
                   :emissions (into {} (for [e candidate/emissions-types
                                             :let [em (candidate/emissions candidate e instance)]
                                             :when (pos? em)]
                                         [e (float em)]))
                   :connection-cost
                   (float (tariff/connection-cost tariff
                                                  (::demand/kwh candidate)
                                                  (::demand/kwp candidate)))

                   :heat-revenue
                   (float (tariff/annual-heat-revenue tariff
                                                      (::demand/kwh candidate)
                                                      (::demand/kwp candidate)))}))

         (candidate/has-supply? candidate)
         (assoc :supply
                {:capacity-kw (float (::supply/capacity-kwp  candidate 0))
                 :capex       (float (::supply/fixed-cost    candidate 0))
                 "capex/kwp"  (float (::supply/capex-per-kwp candidate 0))
                 "opex/kwp"   (float (::supply/opex-per-kwp  candidate 0))
                 "opex/kwh"   (float (::supply/cost-per-kwh  candidate 0))
                 :emissions   (into {}
                                    (for [e candidate/emissions-types]
                                      [e (float (get-in candidate [::supply/emissions e] 0))]))})))
     
     :edges
     (for [edge (->> (graph/edges net-graph)
                     (map (comp vec sort))
                     (set))]
       {:i (first edge) :j (second edge)
        ;; :components (vec (attr/attr net-graph edge :ids))
        :length (float (or (attr/attr net-graph edge :length) 0))
        :fixed-cost (float (or (attr/attr net-graph edge :fixed-cost) 0))
        :variable-cost (float (or (attr/attr net-graph edge :variable-cost) 0))
        :bounds (edge-bounds edge)
        ;; the above may seem odd (dividing the cost - didn't we multiply it?)
        ;; this is because we want a unit cost for the edge.
        :required (boolean (attr/attr net-graph edge :required))})
     }))

(defn- index-by [f vs]
  (reduce #(assoc %1 (f %2) %2) {} vs))

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

        update-vertex (fn [v]
                        (let [solution-vertex (solution-vertices (::candidate/id v))]
                          (cond-> (assoc v ::solution/included true)
                            ;; demand facts
                            (:heat-revenue solution-vertex)
                            (assoc ::solution/heat-revenue   (:heat-revenue solution-vertex)
                                   ::solution/connection-cost (:connection-cost solution-vertex)
                                   ::solution/avoided-emissions (:avoided-emissions solution-vertex))

                            ;; supply facts
                            (:capacity-kw solution-vertex)
                            (assoc ::solution/capacity-kw (:capacity-kw solution-vertex)
                                   ::solution/diversity   (:diversity solution-vertex)
                                   ::solution/output-kwh  (:output-kwh solution-vertex)
                                   ::solution/principal   (:principal solution-vertex)
                                   ::solution/opex        (:opex solution-vertex)
                                   ::solution/heat-cost   (:heat-cost solution-vertex)
                                   ::solution/emissions   (:emissions solution-vertex)
                                   )
                            )))
        
        update-edge   (fn [e]
                        (let [solution-edge (solution-edges (::candidate/id e))
                              candidate-length (::path/length e)
                              input-length (or (attr/attr net-graph [(:i solution-edge) (:j solution-edge)] :length) candidate-length)
                              
                              length-factor (if (zero? candidate-length) 0 (/ candidate-length input-length))
                              ]

                          
                          
                          (assoc e
                                 ::solution/length-factor length-factor
                                 ::solution/included true
                                 ::solution/diameter-mm (:diameter-mm solution-edge)
                                 ::solution/capacity-kw (:capacity-kw solution-edge)
                                 ::solution/diversity   (:diversity solution-edge)
                                 ::solution/principal   (* length-factor (:principal solution-edge))
                                 ::solution/losses-kwh  (* HOURS-PER-YEAR length-factor (:losses-kw solution-edge)))
                          ))
        ]
    (if (= state :error)
      instance

      (-> instance
          (document/map-candidates update-vertex (keys solution-vertices))
          (document/map-candidates update-edge (keys solution-edges))
          (assoc ::solution/objective (:objective result-json)
                 ::solution/finance-parameters
                 (select-keys instance
                              [::document/npv-rate
                               ::document/npv-term
                               ::document/loan-rate
                               ::document/loan-term]))))))

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
                                 (filter candidate/is-included?))

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

              _ (println "Solver ran in" (- end-time start-time) "ms")

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
          (spit (io/file working-directory "instance.edn") solved-instance)
          solved-instance)))
    
    ;; write the scenario down
    ))

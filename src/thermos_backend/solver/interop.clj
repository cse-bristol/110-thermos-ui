(ns thermos-backend.solver.interop
  (:require [clojure.java.io :as io]
            [org.tobereplaced.nio.file :as nio]

            [clojure.java.shell :refer [sh]]
            [clojure.edn :as edn]
            
            [clojure.data.json :as json]
            [clojure.set :as set]
            [clojure.string]

            [loom.graph :as graph]
            [loom.attr :as attr]
            
            [thermos-specs.document :as document]
            [thermos-specs.solution :as solution]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.supply :as supply]
            [thermos-specs.path :as path])
  (:import [java.io StringWriter]))

(def HOURS-PER-YEAR 8766)

(defn- annual-kwh->kw [kwh-pa]
  (/ kwh-pa HOURS-PER-YEAR))

(defn- simplify-topology
  "For CANDIDATES, create a similar graph in which all vertices of degree two
  that don't represent demand or supply points have been collapsed.

  You should restrict CANDIDATES to the included candidates first.

  The output graph has edge labels :ids, which relate to a collection of
  candidate path IDs that are included by using that edge.

  TODO this could remove junction vertices which are not needed to
  reach a demand from a supply.
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

        ;; tag all the edges with their path IDs
        net-graph (reduce (fn [g p]
                            (attr/add-attr g (::path/start p) (::path/end p)
                                           :ids #{(::candidate/id p)}))
                          net-graph
                          paths)

        collapse-junction ;; this is a function to take a graph and
        ;; delete a node, preserving the identiy
        ;; information on the edges. This will later
        ;; let us emit the edges into the output
        ;; usefully.
        (fn [graph node]
          (let [edges (graph/out-edges graph node)
                all-ids (set (mapcat #(attr/attr graph % :ids) edges))
                ;; since an edge is just a tuple, apply concat edges gives me
                ;; e.g. [a b] [b c] => [a b c]
                ;; we only call this when there are exactly two edges
                ;; so deleting b gives us [a c] which is our new edge
                new-edge (vec (remove (partial = node) (apply concat edges)))
                ]
            (-> graph
                (graph/remove-nodes node) ;; delete this node (and edges)
                (graph/add-edges new-edge) ;; introduce a bridging edge
                (attr/add-attr new-edge :ids all-ids) ;; store the original edges for it
                )))

        ;; collapse all collapsible edges until we have finished doing so.
        net-graph
        (loop [net-graph net-graph]
          (let [collapsible (->> (graph/nodes net-graph)
                                 (filter #(not (attr/attr net-graph % :real-vertex)))
                                 (filter #(= 2 (graph/out-degree net-graph %))))]
            (if (empty? collapsible)
              net-graph
              ;; this should be OK because we are working on nodes.
              ;; removing one collapsible node does not make another
              ;; collapsible node invalid.
              (recur (reduce collapse-junction net-graph collapsible)))))
        ]
    net-graph))

(defn- summarise-attributes
  "Take CANDIDATES and NET-GRAPH being a loom graph with :ids on some edges,
  and add :cost, :length :requirement onto those edges being the total
  cost, length and requirement of corresponding paths in CANDIDATES

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

    ;; this won't be quite right with kw-cost varying
    (reduce (fn [g e]
              (let [path-ids (attr/attr g e :ids)]
                (-> g
                    (attr/add-attr e :fixed-cost (total-value path-ids ::path/cost-per-m))
                    (attr/add-attr e :length (total-value path-ids ::path/length))
                    (attr/add-attr e :requirement (total-requirement path-ids))
                    )))
            net-graph (graph/edges net-graph))))

(defn- instance->json [instance net-graph]
  (let [candidates     (::document/candidates instance)
        global-factors (::demand/emissions instance)
        default-price  (float (::demand/price instance))]
    {:finance
     {:loan-term  (int (::document/loan-term instance))
      :loan-rate  (float (::document/loan-rate instance))
      :npv-term   (int (::document/npv-term instance))
      :npv-rate   (float (::document/npv-rate instance))
      :heat-price default-price}

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
                {:kw        (float   (annual-kwh->kw (::demand/kwh candidate 0)))
                 :kwp       (float   (::demand/kwp candidate (::demand/kwh candidate 0)))
                 :required  (boolean (candidate/required? candidate))
                 :count     (int     (::demand/connection-count candidate 1))
                 :emissions (into {} (for [e candidate/emissions-types
                                           :let [d (::demand/kwh candidate 0)
                                                 f (get (::demand/emissions candidate) e
                                                        (global-factors e 0))
                                                 em (* d f)]
                                           :when (pos? em)]
                                       [e  (float em)]))
                 :heat-price (float (::demand/price candidate default-price))})

         (candidate/has-supply? candidate)
         (assoc :supply
                {:capacity-kw (float (::supply/capacity-kwp  candidate 0))
                 :capex       (float (::supply/fixed-cost    candidate 0))
                 "capex/kwp"  (float (::supply/capex-per-kwp candidate 0))
                 "opex/kwp"   (float (::supply/opex-per-kwp  candidate 0))
                 "opex/kwh"   (float (::supply/opex-per-kwh  candidate 0))
                 :emissions   (into {}
                                    (for [e candidate/emissions-types]
                                      [e (float (get-in candidate [::supply/emissions e] 0))]))})))
     
     :edges
     (for [edge (->> (graph/edges net-graph)
                     (map (comp vec sort))
                     (set))]
       {:i (first edge) :j (second edge)
        :length (float (or (attr/attr net-graph edge :length) 0))
        :cost (float (or (attr/attr net-graph edge :fixed-cost) 0))
        :required (boolean (attr/attr net-graph edge :required))})
     }))

(defn- index-by [f vs]
  (reduce #(assoc %1 (f %2) %2) {} vs))

(defn- merge-solution [instance net-graph result-json]
  instance
  ;; (let [state (keyword (:state result-json))

  ;;       ;; add the solution state, that's always useful:
  ;;       instance (assoc-in instance [::solution/summary ::solution/state] state)]
    
  ;;   (if (= state :error)
  ;;     (assoc-in instance
  ;;               [::solution/summary ::solution/message] (:message result-json))

  ;;     ;; otherwise not an error
      
  ;;     (let [demands-by-id  (index-by :id (:demands result-json))
  ;;           supplies-by-id (index-by :id (:supplies result-json))
  ;;           edges-by-id

  ;;           (into {}
  ;;                 (mapcat
  ;;                  (fn [{s :src d :dst :as info}]
                     
  ;;                    (let [length (attr/attr net-graph [s d] :length)]
  ;;                      (for [e (attr/attr net-graph [s d] :ids)]
  ;;                        [e (assoc info :length length)])))
  ;;                  (:edges result-json)))
  ;;           clean-plant
  ;;           (fn [{id :id
  ;;                 count :count
  ;;                 cap :capacity
  ;;                 cost :capital-cost
  ;;                 fuel :fuel-kwh
  ;;                 fuel-cost :fuel-cost
  ;;                 heat :heat-kwh
  ;;                 power :power-kwh
  ;;                 }]
  ;;             (merge
  ;;              {::technology/id id
  ;;               ::solution/count count
  ;;               ::solution/capacity cap
  ;;               ::solution/capital-cost cost
  ;;               ::solution/heat-output heat
  ;;               ::solution/fuel-input fuel
  ;;               ::solution/fuel-cost fuel-cost}
  ;;              (when power {::solution/power-output power})))
  ;;           ]
  ;;       (-> instance
  ;;           ;; copy the more detailed details
            
  ;;           (document/map-candidates
  ;;            #(let [id (::candidate/id %)]
  ;;               (candidate/add-building-to-solution
  ;;                %
  ;;                :heat-revenue   (get-in demands-by-id [id :revenue])
  ;;                :power-revenue  (get-in supplies-by-id [id :grid-revenue])
  ;;                :plant          (map clean-plant (get-in supplies-by-id [id :plant]))))
             
  ;;            (concat (keys demands-by-id) (keys supplies-by-id)))

  ;;           (document/map-candidates
  ;;            #(let [id (::candidate/id %)
  ;;                   edge-info (edges-by-id id)
  ;;                   total-length (:length edge-info)
  ;;                   ;; pro-rata by length is wrong
                    
  ;;                   ;; the only real answer is to either merge the
  ;;                   ;; costs in some way in the solution, or to
  ;;                   ;; recompute the values used and reapply them
  ;;                   ;; here.

  ;;                   ;; maybe that is the best idea? alternatively
  ;;                   ;; could give the opti. model a bunch of
  ;;                   ;; components to add up for the edge, which it can
  ;;                   ;; then produce separate answers for
                    
  ;;                   ;; that might be nicer.
  ;;                   ]
  ;;               (candidate/add-path-to-solution
  ;;                %
  ;;                :capital-cost (:capital-cost edge-info)
  ;;                :capacity (:capacity edge-info)))
             
  ;;            (keys edges-by-id))

  ;;           (assoc-in [::solution/summary ::solution/objective-value]
  ;;                     (:objective result-json)))

  ;;       )))
  )


(defn solve
  "Solve the INSTANCE, returning an updated instance with solution
  details in it. Probably needs running off the main thread."
  [label config instance]
  
  (let [instance (document/remove-solution instance) ;; throw away
        ;; any existing
        ;; solution

        working-directory (nio/path (config :solver-directory))

        _ (.mkdirs (.toFile working-directory))
        
        working-directory (.toFile (nio/create-temp-directory! working-directory label))
        
        input-file (io/file working-directory "problem.json")

        solver-command (config :solver-command)

        included-candidates (->> (::document/candidates instance)
                                 (vals)
                                 (filter candidate/is-included?))

        net-graph (simplify-topology included-candidates)
        net-graph (summarise-attributes net-graph included-candidates)

        ;; This is now the topology we want. Every edge may be several
        ;; input edges, and nodes can either be real ones or junctions
        ;; that are needed topologically.

        ;; Edges can have attributes :ids, which say the input paths,
        ;; and :cost which say the total pipe cost for the edge.

        input-json (instance->json instance net-graph)
        ]
    ;; write the scenario down
    (with-open [writer (io/writer input-file)]
      (json/write input-json writer :escape-unicode false))
        
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
              (assoc ::solution/summary
                     #::solution {:runtime (- end-time start-time)
                                  :log [(:out output) (:err output)]
                                  :state (:state output-json)
                                  })
              (merge-solution net-graph output-json))
          
          ]
      (spit (io/file working-directory "instance.edn") solved-instance)
      solved-instance
      )))

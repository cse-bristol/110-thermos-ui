(ns thermos-backend.solver.interop
  (:require [clojure.java.io :as io]
            [org.tobereplaced.nio.file :as nio]
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
            [thermos-backend.solver.bounds :as bounds])
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

        cost-of-path
        (into {} (for [path paths] [(::candidate/id path) (path/cost path)]))
        
        cost-of-paths (fn [ids] (reduce + 0 (map cost-of-path ids)))
        
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
                graph (-> graph
                          (graph/remove-nodes node)
                          (graph/add-edges new-edge))

                existing-ids (attr/attr graph new-edge :ids)
                ]
            (if (or (empty? existing-ids)
                    (< (cost-of-paths all-ids)
                       (cost-of-paths existing-ids)))
              (attr/add-attr graph new-edge :ids all-ids)
              graph)))

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

    (reduce (fn [g e]
              (let [path-ids (attr/attr g e :ids)]
                (-> g
                    (attr/add-attr e :fixed-cost (total-value path-ids path/cost))
                    (attr/add-attr e :length (total-value path-ids ::path/length))
                    (attr/add-attr e :requirement (total-requirement path-ids))
                    )))
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
        
        global-factors (::demand/emissions instance)
        default-price  (float (::demand/price instance))]
    
    {:time-limit (float (::document/maximum-runtime instance 1.0))
     :mip-gap    (float (::document/mip-gap instance 0.05))
     
     :finance
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
                                           :let [em (candidate/emissions candidate e instance)]
                                           :when (pos? em)]
                                       [e (float em)]))
                 :heat-price (float (::demand/price candidate default-price))})

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
        :cost (float (let [cost (attr/attr net-graph edge :fixed-cost)
                           length (attr/attr net-graph edge :length)]
                       (if (zero? cost) (float cost)
                           (/ (float cost) (float length)))))
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
                        (let [solution-edge (solution-edges (::candidate/id e))]
                          (assoc e
                                 ::solution/included true
                                 ::solution/capacity-kw (:capacity-kw solution-edge)
                                 ::solution/diversity   (:diversity solution-edge)
                                 ::solution/principal   (:principal solution-edge)
                                 ::solution/losses-kwh  (* HOURS-PER-YEAR (:losses-kw solution-edge))
                                 )))
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

(defn- mark-unreachable [instance net-graph components]
  (let [vertices (set (apply concat components))

        paths
        (->> (graph/edges net-graph)
             (filter (fn [[i j]] (or (vertices i) (vertices j)) ))
             (mapcat #(attr/attr net-graph % :ids))
             (set))
        
        candidates (::document/candidates instance)
        is-candidate (fn [id] (contains? candidates id))
        buildings (filter is-candidate vertices)]
    
    (document/map-candidates
     instance
     #(assoc % ::solution/unreachable true)
     (concat paths buildings))))

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

        ;; at this point we should check for some more bad things:

        ;; * components not connected to any supply vertex
        ccs (map set (graph-alg/connected-components net-graph))
        supplies (map ::candidate/id (filter candidate/has-supply? included-candidates))

        invalid-ccs (filter (fn [cc] (not-any? cc supplies)) ccs)

        _ (log/info "removing" (count invalid-ccs) "un-suppliable components"
                    "containing" (reduce + 0 (map count invalid-ccs)) "vertices")

        net-graph-with-unreachable net-graph
        
        net-graph (reduce (fn [g cc] (graph/remove-nodes* g cc))
                          net-graph invalid-ccs)
        
        ;; * edges to nowhere
        
        
        ;; This is now the topology we want. Every edge may be several
        ;; input edges, and nodes can either be real ones or junctions
        ;; that are needed topologically.

        ;; Edges can have attributes :ids, which say the input paths,
        ;; and :cost which say the total pipe cost for the edge.
        
        input-json (instance->json instance net-graph)
        ]
    ;; write the scenario down
    (log/info "Output scenario to %s" input-file)
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
              (mark-unreachable net-graph-with-unreachable invalid-ccs))
          
          ]
      (spit (io/file working-directory "instance.edn") solved-instance)
      solved-instance
      )))

(ns thermos-ui.backend.solver.interop
  (:require [clojure.java.io :as io]
            [org.tobereplaced.nio.file :as nio]

            [clojure.java.shell :refer [sh]]
            [clojure.edn :as edn]
            
            [clojure.data.json :as json]
            [clojure.set :as set]
            [clojure.string]

            [loom.graph :as graph]
            [loom.attr :as attr]
            
            [thermos-ui.specs.document :as document]
            [thermos-ui.specs.solution :as solution]
            [thermos-ui.specs.technology :as technology]
            [thermos-ui.specs.candidate :as candidate]

            [thermos-ui.frontend.operations :as operations])
  (:import [java.io StringWriter]))

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

  (let [{paths :path
         buildings :building
         }
        (group-by ::candidate/type candidates)

        net-graph (apply
                   graph/graph
                   (concat
                    (map ::candidate/connection buildings)

                    (map ::candidate/path-start paths)
                    (map ::candidate/path-end paths)
                    
                    (map ::candidate/id buildings)

                    (map #(vector (::candidate/path-start %) (::candidate/path-end %)) paths)
                    (map #(vector (::candidate/connection %) (::candidate/id %)) buildings)))

        _ (println "net-graph has" (count (graph/nodes net-graph)) "vertices at start")
        
        ;; tag all the real vertices
        net-graph (reduce (fn [g d]
                            (attr/add-attr g (::candidate/id d) :real-vertex true))
                          net-graph
                          buildings)

        ;; tag all the edges with their path IDs
        net-graph (reduce (fn [g p]
                            (attr/add-attr g (::candidate/path-start p) (::candidate/path-end p)
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
            :optional
            ))
        ]
    
    (reduce (fn [g e]
              (let [path-ids (attr/attr g e :ids)]
                (-> g
                    (attr/add-attr e :fixed-cost (total-value path-ids ::candidate/path-cost))
                    (attr/add-attr e :kw-cost 0) ;; TODO variable cost in model
                    (attr/add-attr e :length (total-value path-ids ::candidate/length))
                    (attr/add-attr e :requirement (total-requirement path-ids))
                    )))
            net-graph (graph/edges net-graph))))

(def HOURS-PER-YEAR 8766)

(defn- instance->json [technologies candidates net-graph]
  {:periods [{:duration HOURS-PER-YEAR}]
   :heat-price 1
   :export-price 1

   :finance {:network {:term 1 :rate 1}
             :plant {:term 1 :rate 1}}

   :emissions {:co2e {:cost 1}}
   
   :fuels {:gas {:price 1
                 :emissions {:co2e 1}}}

   :plant
   (into {}
         (for [tech technologies]
           [(::technology/id tech)
            {:capacity 100
             :fuel-input 1
             :heat-output 1
             :power-output 1
             :fuel :gas
             :capital-cost 100}
            ]
           ))
   
   :edges
   (for [edge (->> (graph/edges net-graph) ;; take each directed edge
                   (map (comp vec sort)) ;; convert it into an undirected edge
                   (set))] ;; remove duplicate undirected edges
     {:src (first edge)
      :dst (second edge)
      :fixed-cost (attr/attr net-graph edge :fixed-cost)
      :kw-cost (attr/attr net-graph edge :kw-cost)
      :required (boolean (attr/attr net-graph edge :required))})
   
   :vertices
   (into
    {}
    (for [node (graph/nodes net-graph)
          :let [candidate (candidates node)]]
      [node
       (cond-> {}
         (candidate/is-demand? candidate) (assoc :demand [(/ (candidate/annual-demand candidate) HOURS-PER-YEAR)])
         (candidate/is-required? candidate) (assoc :required true)
         (candidate/is-supply? candidate) (assoc :supply
                                                 (reduce-kv #(assoc %1 %2 [(:min %3) (:max %3)]) {}
                                                            (::candidate/allowed-technologies candidate))
                                                 :grid {} ;; TODO grid infos
                                                 )
         )]))
   })


(defn solve
  "Solve the INSTANCE, returning an updated instance with solution
  details in it. Probably needs running off the main thread."
  [label config instance]
  
  (let [instance (operations/remove-solution instance) ;; throw away
        ;; any existing
        ;; solution

        working-directory (nio/path (config :solver-directory))
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

        input-json (instance->json (::document/technologies instance)
                                   (::document/candidates instance) net-graph)
        ]
    ;; write the scenario down
    (with-open [writer (io/writer input-file)]
      (json/write input-json writer :escape-unicode false))
        
    ;; invoke the solver
    (let [start-time (System/currentTimeMillis)
          output (sh solver-command "problem.json" "solution.json"
                     :dir working-directory)
          
          end-time (System/currentTimeMillis)

          solution {::solution/runtime (- end-time start-time)
                    ::solution/log (str (:out output) "\n" (:err output))
                    }

          _ (println "Solver ran in" (- end-time start-time) "ms")

          output-json (try
                        (with-open [r (io/reader (io/file working-directory "solution.json"))]
                          (json/read r :key-fn keyword))
                        (catch Exception ex
                          {:error (.getMessage ex)}))
          ]
      ;; todo process this more neatly
      (assoc instance
             ::solution/solution (assoc solution :json output-json)
             
             )
      ))

  )
    


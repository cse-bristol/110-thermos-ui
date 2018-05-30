(ns thermos-ui.backend.solver.interop
  (:require [clojure.java.io :as io]
            [org.tobereplaced.nio.file :as nio]
            
            [clojure.java.shell :refer [sh]]
            [clojure.edn :as edn]
            [yaml.core :as yaml]
            [clojure.data.csv :as csv]

            [loom.graph :as graph]
            [loom.attr :as attr]
            
            [thermos-ui.specs.document :as document]
            [thermos-ui.specs.solution :as solution]
            [thermos-ui.specs.candidate :as candidate]

            [thermos-ui.frontend.operations :as operations]
            ))

;; TODO candidates with several connection points are not handled.

(defn- read-csv-map [reader]
  (let [rows (csv/read-csv reader)
        header (map keyword (first rows))
        rows (rest rows)
        ]
    (map #(into {} (map vector header %)) rows)))

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
         demands :demand
         supplies :supply}
        (group-by ::candidate/type candidates)

        _ (println (count paths) (count demands) (count supplies)
                   (count candidates)
                   (keys (group-by ::candidate/type candidates))
                   )
        
        net-graph (apply
                   graph/graph
                   (concat
                    (map ::candidate/connection demands)
                    (map ::candidate/connection supplies)
                    (map ::candidate/path-start paths)
                    (map ::candidate/path-end paths)
                    
                    (map ::candidate/id demands)
                    (map ::candidate/id supplies)

                    (map #(vector (::candidate/path-start %) (::candidate/path-end %)) paths)
                    (map #(vector (::candidate/connection %) (::candidate/id %)) demands)
                    (map #(vector (::candidate/connection %) (::candidate/id %)) supplies)))

        _ (println "net-graph has" (count (graph/nodes net-graph)) "vertices at start")
        
        ;; tag all the real vertices
        net-graph (reduce (fn [g d]
                            (attr/add-attr g (::candidate/id d) :real-vertex true))
                          net-graph
                          (concat demands supplies))

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
             (filter #(= :path (::candidate/type %)))
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
                    (attr/add-attr e :cost (total-value path-ids ::candidate/path-cost))
                    (attr/add-attr e :length (total-value path-ids ::candidate/length))
                    (attr/add-attr e :requirement (total-requirement path-ids))
                    )))
            net-graph (graph/edges net-graph))))

(defn- kWh_year->MW [val]
  (* val 0.0000001140771161305))

(defn solve
  "Solve the INSTANCE, returning an updated instance with solution
  details in it. Probably needs running off the main thread."
  [label config instance]
  
  (let [instance (operations/remove-solution instance) ;; throw away
                                                       ;; any existing
                                                       ;; solution

        working-directory (nio/path (config :solver-directory))

        default-scenario (edn/read-string
                          (slurp
                           (io/reader (io/resource "default-scenario.edn"))))

        solver-command (config :solver-command)

        included-candidates (->> (::document/candidates instance)
                                 (vals)
                                 (filter (comp #{:optional :required} ::candidate/inclusion)))

        _ (def last-included-candidates included-candidates)
        
        net-graph (simplify-topology included-candidates)
        net-graph (summarise-attributes net-graph included-candidates)

        _ (def last-net-graph net-graph)
        
        ;; This is now the topology we want. Every edge may be several
        ;; input edges, and nodes can either be real ones or junctions
        ;; that are needed topologically.

        ;; Edges can have attributes :ids, which say the input paths,
        ;; and :cost which say the total pipe cost for the edge.

        ;; At this point we can hopefully just write out the rest of
        ;; the stuff

                
        working-directory (.toFile (nio/create-temp-directory! working-directory label))


        put-csv (fn [filename & rows]
                  (let [file (io/file working-directory filename)]
                    (with-open [writer (io/writer file)]
                      (doseq [r rows]
                        (csv/write-csv writer r)))
                    
                    file))

        cityspace-csv
        (put-csv
         "cityspace.csv"
         [["id" "Area" "X" "Y" "Exclude" "isHinterland" "Households"]]
         (for [node (graph/nodes net-graph)]
           [node 1000 0 0 0 "n" 1]))
        
        demands-csv
        (put-csv
         "demands.csv"
         ;; [[id period resource demand requirement]]
         (for [{id ::candidate/id type ::candidate/type demand ::candidate/demand
                requirement ::candidate/inclusion} included-candidates
               :when (= :demand type)]
           
           [id "average" "heat" (kWh_year->MW demand) (if (= :optional requirement) "0" "1")]))

        ;; The edge set contains both directions for edges, but we
        ;; only want one direction. Doesn't matter which one
        distinct-edges
        (set (map set (graph/edges net-graph)))

        neighbours-csv
        (put-csv
         "neighbours.csv"
         ;; [[from to major_period length]]
         (for [edge distinct-edges]
           [(first edge) (second edge) 1 (or (attr/attr net-graph (vec edge) :length) 0)]))

        edge-details
        (put-csv
         "edge-details.csv"
         ;; [[from to requirement cost]]
         (for [edge distinct-edges]
           [(first edge) (second edge)
            (case
                (or (attr/attr net-graph (vec edge) :requirement) :optional)
              :optional 0
              :required 1
              0)
            (or (attr/attr net-graph (vec edge) :cost) 0)
            ]))

        heat-technologies
        (->> default-scenario
             :processes
             (remove :household)
             (filter :includeResflow)
             (map :varname))
        
        process-locations
        (put-csv
         "process-locations.csv"
         ;; [[id process lb ub]]
         (for [{id ::candidate/id type ::candidate/type} included-candidates
               tech heat-technologies
               :when (= :supply type)]
           [id tech 0 10]))
        
        rel-path
        (fn [fi]
          (-> working-directory
              (.toPath)
              (.relativize (.toPath fi))
              (.toString)))
        
        ;; The file names in here should be relative to the working directory
        scenario
        (-> default-scenario
            (assoc-in [:majorperiods 0 :demandfile] (rel-path demands-csv))
            (assoc-in [:cityspace :neighbours] (rel-path neighbours-csv))
            (assoc-in [:cityspace :ncells] (count (graph/nodes net-graph)))
            (assoc-in [:cityspace :gridfile] (rel-path cityspace-csv))

            ;; annoyingly infrastructures is a list rather than a map
            ;; not sure why. 2 is the heat_net infrastructures in default.
            ;; TODO make this right            
            (assoc-in [:infrastructures 2 :required] (rel-path edge-details))

            (assoc-in [:resflow :processLocations] (rel-path process-locations))
            
            (assoc-in [:cityspace :inverseMap] true)
            (assoc-in [:resflow :restrictImports] false)
            (assoc-in [:resflow :restrictSupply] false)
            (assoc-in [:resflow :requireAllDemands] false)

            (assoc-in [:resflow :objectiveFunction] "NPV")

            ;; TODO make this more sensible as well
            (assoc-in [:resflow :tariffs]
                      [{:period "average" :process "heatex"
                        :resource "dist_heat" :value -7}]
                      )

            yaml/generate-string)]
    
    
    ;; write the scenario down
    (spit (io/file working-directory "scenario.yml") scenario)
    
    ;; invoke the solver
    (let [output (sh solver-command
                     "--customdata" "scenario.yml"
                     "--solver" "glpk"
                     "--datadir" "."
                     :dir working-directory)

          _ (spit (io/file working-directory "solver-logfile")
                  (str (:out output)
                       "\n\n"
                       (:err output)
                       ))

          _ (println "Solver completed, log in" working-directory)
          
          read-output (fn [name]
                        (with-open [reader (io/reader (io/file working-directory "out" name))]
                          (doall (read-csv-map reader))))
          
          network-links (read-output "network.csv")
          processes (read-output "process.csv")
          imports (read-output "import.csv")

          path-flows (->> network-links
                          (filter #(= "dist_heat" (:resource %)))
                          (mapcat
                           (fn [{from :from to :to flow :flow}]
                             (let [edge [from to]
                                   flow (Double/parseDouble flow)]
                               (for [in-id (attr/attr net-graph edge :ids)]
                                 [in-id flow]))))
                          (into {}))

          included-vertex-ids (->> processes
                                   (map :cell)
                                   (set)
                                   (filter
                                    (set
                                     (for [{id ::candidate/id
                                            type ::candidate/type}
                                           included-candidates
                                           :when (#{:supply :demand} type)] id))))

          solution
          {::solution/exists true}

          include-candidate #(assoc-in % [::solution/candidate ::solution/included] true)
          
          set-flow #(assoc-in % [::solution/candidate ::solution/heat-flow]
                              (path-flows (::candidate/id %)))
          
          updated-instance
          (-> instance
              (assoc ::solution/solution solution)
              (operations/map-candidates include-candidate (keys path-flows))
              (operations/map-candidates set-flow (keys path-flows))
              (operations/map-candidates include-candidate included-vertex-ids))
          ]

      (spit (io/file working-directory "solved-instance.edn")
            updated-instance) ;; pprint?
      
      updated-instance
      )))

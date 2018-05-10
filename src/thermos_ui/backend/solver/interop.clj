(ns thermos-ui.backend.solver.interop
  (:require [thermos-ui.backend.config :refer [config]]
            [clojure.java.io :as io]
            [org.tobereplaced.nio.file :as nio]
            
            [clojure.java.shell :refer [sh]]
            [clojure.edn :as edn]
            [yaml.core :as yaml]
            [clojure.data.csv :as csv]

            [loom.graph :as graph]
            [loom.attr :as attr]
            
            [thermos-ui.specs.document :as document]
            [thermos-ui.specs.candidate :as candidate]

            [thermos-ui.frontend.operations :as operations]
            )
  )

(def working-directory (nio/path (config :solver-directory)))

(def default-scenario (edn/read-string
                       (slurp
                        (io/reader (io/resource "default-scenario.edn")))))

(def solver-command (config :solver-command))

(defn read-csv-map [reader]
  (let [rows (csv/read-csv reader)
        header (first rows)
        rows (rest rows)
        ]
    (map #(into {} (map vector header %)))))

(defn simplify-topology
  "For CANDIDATES, create a similar graph in which all vertices of degree two
  that don't represent demand or supply points have been collapsed.

  You should restrict CANDIDATES to the included candidates first.

  The output graph has edge labels :ids, which relate to a collection of
  candidate path IDs that are included by using that edge."
  [candidates]

  (let [{paths ::candidate/path
         demands ::candidate/demand
         supplies ::candidate/supply}
        (group-by ::candidate/type candidates)

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
              ;; this should be OK because we are working on nodes
              ;; removing one collapsible node does not make another
              ;; collapsible node invalid.
              (recur (reduce collapse-junction net-graph collapsible)))))
        ]
    net-graph))

(defn sum-path-costs
  "Take CANDIDATES and NET-GRAPH being a loom graph with :ids on some edges,
  and add :cost onto those edges being the total cost of corresponding
  paths in CANDIDATES"
  [net-graph candidates]
  
  (let [paths
        (into {} (filter (comp #{::candidate/path} ::candidate/type) candidates))
        total-cost
        (fn [path-ids]
          (let [costs (map #(or (::candidate/path-cost (paths %)) 0) path-ids)]
            (apply + costs)))
        ]
    (reduce (fn [g e]
              (attr/add-attr g e :cost (total-cost (attr/attr g e :ids))))
            net-graph (graph/edges net-graph))))

(defn solve
  "Solve the INSTANCE, returning an updated instance with solution
  details in it. Probably needs running off the main thread."
  [instance]
  
  (let [included-candidates (->> (::document/candidates instance)
                                 (vals)
                                 (filter (comp #{:optional :required} ::candidate/inclusion)))

        net-graph (simplify-topology included-candidates)
        net-graph (sum-path-costs net-graph included-candidates)
        
        ;; This is now the topology we want. Every edge may be several
        ;; input edges, and nodes can either be real ones or junctions
        ;; that are needed topologically.

        ;; Edges can have attributes :ids, which say the input paths,
        ;; and :cost which say the total pipe cost for the edge.

        ;; At this point we can hopefully just write out the rest of
        ;; the stuff
        
        working-directory (.toFile (nio/create-temp-directory! working-directory "run"))

        cityspace-csv (let [file (io/file working-directory "cityspace.csv")]
                        (with-open [writer (io/writer file)]
                          (csv/write-csv
                           writer
                           [["id" "Area" "X" "Y" "Exclude" "isHinterland" "Households"]])
                          
                          (csv/write-csv
                           writer
                           (for [node (graph/nodes net-graph)]
                             [node 1000 0 0 0 "n" 1])))
                        file)
        
        demands-csv (let [file (io/file working-directory "demands.csv")]
                      (with-open [writer (io/writer file)]
                        (csv/write-csv
                         writer
                         (for [{id ::candidate/id type ::candidate/type demand ::candidate/demand} included-candidates
                               :when (= ::candidate/demand type)]
                           [id "average" "heat" demand])))
                      file)

        neighbours-csv (let [file (io/file working-directory "neighbours.csv")]
                         (with-open [writer (io/writer file)]
                           (csv/write-csv
                            writer
                            (for [edge (graph/edges net-graph)] ;; TODO ensure distinct edges
                              [(first edge) (second edge) 1
                               (or (attr/attr net-graph edge :cost) 0)]
                              )))
                         file)
        
        ;; so this is what conversion technologies can exist in a cell
        ;; I think. TODO generate the conversion technologies off our
        ;; saved state.
        supply-cells (into {}
                           (for [{id ::candidate/id type ::candidate/type} included-candidates
                                 :when (= type ::candidate/supply)]
                             ;; etc
                             [id
                              ["small_chp" "med_chp" "large_chp" "nondom_boiler"]]
                             )
                           )


        ;; I also want to say what cells must be supplied by heat
        ;; exchanger.
        
        ;; and this is what primary resources can flow into a cell
        ;; from outside? I'm not sure it matters for now
        import-cells {} ;;{resource -> [cell]}

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
            (assoc-in [:cityspace :gridfile] (rel-path cityspace-csv))
            (assoc-in [:resflow :restrictImports] false) ;; if true we need something in import-cells
            (assoc-in [:resflow :supplyCells] supply-cells)
            (assoc-in [:resflow :restrictSupply] true)
            (assoc-in [:resflow :importCells] import-cells)
            yaml/generate-string)]
    
    ;; write the scenario down
    (spit (io/file working-directory "scenario.yml") scenario)
    (spit (io/file working-directory "scenario.dat")
          ;; in some cases this varies and changes some params
          ;; but I am hoping that doesn't matter
          "set M := capex opex ghg;\n" 
          )
    ;; invoke the solver
    (println (sh solver-command
                 "--scenario" "scenario.yml"
                 "--solver" "glpk"
                 "--datadir" "."
                 :dir working-directory))

    (let [network-file (io/file working-directory "network.csv")
          network-links (with-open [reader (io/reader network-file)]
                          ;; we need to discard the first line as it doesn't help
                          (.readline reader)
                          (doall (read-csv-map reader)))

          processes (with-open [reader (io/reader)]
                      (.readline reader)
                      (doall (read-csv-map reader)))

          included-path-ids (->> network-links
                                 (filter #(= "dist_heat" (% "resource")))
                                 (map #(vector (% "from") (% "to")))
                                 (mapcat #(attr/attr net-graph % :ids))
                                 (set))

          included-vertex-ids (->> processes
                                   (filter #(= "heatex" (% "process")))
                                   (map #(get % "cell"))
                                   (set))
          ]

      ;; there's other information to bring in here
      
      (operations/map-candidates
       instance
       #(assoc % ::candidate/in-solution true)
       (concat included-path-ids included-vertex-ids)
       )
      )))

(ns thermos-ui.backend.solver
  (:require [thermos-ui.backend.config :refer config]
            [clojure.java.io :as io]
            [clojure.java.sh :refer [sh]]
            [clojure.edn :as edn]
            [yaml.core :as yaml]
            [clojure.data.csv :as csv]

            [loom.graph :as graph]
            [loom.attr :as attr]
            
            [thermos-ui.specs.document :as document]
            [thermos-ui.specs.candidate :as candidate]
            )
  
  (:import [java.nio.file Files Paths]))

(def working-directory (Paths/get (config :solver-directory)))
(def default-scenario (edn/read-string
                       (slurp
                        (io/reader (io/resource "default-scenario.edn")))))

(def solver-command (config :solver-command))

(defn solve
  "Solve the INSTANCE, returning an updated instance with solution
  details in it. Probably needs running off the main thread."
  [instance]

  ;; make a new working directory for the solver
  ;; - create a scenario file
  ;; - create the network data for the scenario
  ;; - run the solver
  ;; - read the outputs
  ;; - delete the working directory
  
  (let [included-candidates (-> (::document/candidates doc)
                                (vals)
                                (filter #(#{:required :optional}
                                          (::candidate/inclusion %))))

        {paths ::candidate/path
         demands ::candidate/demand
         supplies ::candidate/supply}
        (group-by ::candidate/type included-candidates)

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
                                 (filter #(not (attr/attr graph % :real-vertex)))
                                 (filter #(= 2 (graph/out-degree net-graph %))))]
            (if (empty? collapsible)
              net-graph
              ;; this should be OK because we are working on nodes
              ;; removing one collapsible node does not make another
              ;; collapsible node invalid.
              (recur (reduce collapse-junction net-graph collapsible)))))
                

        ;; This is now the topology we want. Every edge may be several
        ;; input edges, and nodes can either be real ones or junctions
        ;; for topo. Anyway we need to write it out, and run the model
        ;; on it. Then we can use the topo to connect it back up
        ;; again.

        
        
        ;; we need to be able to add up the cost attribute for merged paths:

        paths (into {} (for [path paths] [(::candidate/id path) path]))
        sum-path-costs (fn [ids]
                         (apply +
                                (map
                                 #(or (::candidate/path-cost (paths %)) 0)
                                 ids)))
        
        working-directory (Files/createTempDirectory working-directory "run")

        cityspace-csv (let [file (io/file working-directory "cityspace.csv")]
                        (with-open [writer (io/writer file)]
                          (csv/write-csv
                           [["id" "Area" "X" "Y" "Exclude" "isHinterland" "Households"]])
                          
                          (csv/write-csv
                           writer
                           (for [node (graph/nodes net-graph)]
                             [node 1000 0 0 0 "n" 1])))
                        file)
        
        demands-csv (let [file (io/file working-directory "demands.csv")]
                      (with-open [writer (io/writer file)]
                        (csv/write-csv
                         (for [demand demands]
                           [(::candidate/id demand) "average" "heat" (::candidate/demand demand)])))
                      file)

        neighbours-csv (let [file (io/file working-directory "neighbours.csv")]
                         (with-open [writer (io/writer file)]
                           (csv/write-csv
                            (for [edge (graph/edges net-graph)] ;; TODO ensure distinct edges
                              [(first edge) (second edge) 1
                               (sum-path-costs (attr/attr net-graph edge :ids))])))
                         file) ;; this gives connectivity for
                                        ;; vertices

        ;; so this is what conversion technologies can exist in a cell
        ;; I think. TODO generate the conversion technologies off our
        ;; saved state.
        supply-cells (into {}
                           (for [supply supplies]
                             ;; etc
                             [(::candidate/id supply)
                              ["small_chp" "med_chp" "large_chp" "nondom_boiler"]]
                             )
                           )
        ;; and this is what primary resources can flow into a cell
        ;; from outside?
        import-cells {} ;;{resource -> [cell]}

        ;; TODO do these file names need relativising
        scenario
        (-> default-scenario
            (assoc-in [:majorperiods 0 :demandfile] demands-csv)
            (assoc-in [:cityspace :neighbours] neighbours-csv)
            (assoc-in [:cityspace :gridfile] cityspace-csv)
            (assoc-in [:resflow :restrictImports] true)
            (assoc-in [:resflow :supplyCells] supply-cells)
            (assoc-in [:resflow :restrictSupply] true)
            (assoc-in [:resflow :importCells] import-cells)
            yaml/generate-string)]
    
    ;; write the scenario down
    (spit (io/file working-directory "scenario.yml" scenario))
    ;; invoke the solver
    (sh solver-command
        "--scenario" "scenario.yml"
        "--solver" "glpk"
        "--datadir" "."
        :dir working-directory)
    ;; read the results and do something useful with them
    )
  )

(ns thermos-ui.backend.solver.interop
  (:require [clojure.java.io :as io]
            [org.tobereplaced.nio.file :as nio]
            
            [clojure.java.shell :refer [sh]]
            [clojure.edn :as edn]
            [yaml.core :as yaml]
            
            [clojure.data.json :as json]
            [clojure.data.csv :as csv]
            [clojure.set :as set]
            [clojure.string]

            [loom.graph :as graph]
            [loom.attr :as attr]
            
            [thermos-ui.specs.document :as document]
            [thermos-ui.specs.solution :as solution]
            [thermos-ui.specs.technology :as technology]
            [thermos-ui.specs.candidate :as candidate]

            [thermos-ui.frontend.operations :as operations]))

(def default-scenario
  "An EDN representation of a typical YML scenario for the SEM program."
  (edn/read-string
   (slurp
    (io/reader (io/resource "default-scenario.edn")))))

(def household-processes
  "Processes for 'household' use taken from the default scenario"
  (filter
   :household
   (:processes default-scenario)))

(def supply-process-template
  "The default parameters for a supply process to be put into the output
  YML"
  {:maintenanceCost 0
   :capitalCost 0,
   :household false,
   :networkType "",
   :maxAllowed 1000,
   :operatingCost 0.0,
   :period 0, :discountRate 0.035,
   :minRate 0,
   :maxRate 0,
   :includeResflow true,
   :operatingEmissions 0.0}
  )

(def resource-names
  "Maps from the values of ::technology/fuel to strings used
  to define resources in default-scenario.edn"
  {:gas "gas"
   :electricity "elec"
   :biomass "chips"})

(defn technology->supply-process
  "Takes a technology, defined per things in the technology spec.
  Outputs something suitable for putting into the processes list in
  the SEM input files."
  [tech]
  (let [power-efficiency (or (::technology/power-efficiency tech) 0)
        heat-efficiency (or (::technology/heat-efficiency tech) 1)

        heat-led (= 0 power-efficiency)
        leading-efficiency (if heat-led heat-efficiency power-efficiency)
        
        ;; if we have an efficiency of 80%, that means that for 1 unit
        ;; of output, our input flow is 1/80%

        ;; our capacity is the nameplate output capacity. According to kamal
        ;; the process' operating level (P) is what is bounded. Output is P*
        ;; output rate and input is P*input rate. So this is ok.
        
        input-flow
        {:flowValue (float (/ 1 leading-efficiency))
         :resourceName (resource-names
                        (::technology/fuel tech))}
        
        output-flows
        (remove
         (comp zero? :flowValue)

         [{:flowValue (float (/ heat-efficiency leading-efficiency))
           :resourceName "dist_heat"}
          {:flowValue (float (/ power-efficiency leading-efficiency))
           :resourceName "elec"}])
        ]
    
    (merge supply-process-template
           {:varname (::technology/id tech)
            :capitalCost (float (/ (::technology/capital-cost tech)
                                   1000))
            
            :maxRate (::technology/capacity tech) ;; in MW
            
            :inputFlows [input-flow]
            :outputFlows output-flows})))

(defn add-processes-from [output instance]
  (assoc output :processes
         (->> instance
              (::document/technologies)
              (map technology->supply-process)
              (concat household-processes)
              (vec))))

(defn set-prices-from [output instance]
  ;; setup price contributions and other objective parameters
  (let [number-of-candidates (count (::document/candidates instance))

        {{plant-period ::document/plant-period
          plant-interest-rate ::document/plant-interest-rate

          network-period ::document/network-period
          network-interest-rate ::document/network-interest-rate
          
          carbon-cost ::document/carbon-cost
          carbon-cap ::document/carbon-cap
          gas-price ::document/gas-price
          biomass-price ::document/biomass-price
          electricity-in-price ::document/electricity-import-price
          electricity-out-price ::document/electricity-export-price
          heat-price ::document/heat-price

          electricity-emissions ::document/electricity-emissions
          gas-emissions ::document/gas-emissions
          biomass-emissions ::document/biomass-emissions
          
          } ::document/objective} instance
        ;; the optimiser takes tariffs in kMoney/MWh
        ;; so this is money/kwh, so divide 100 is p/kwh
        ;; similarly for prices
        plant-interest-rate   (float (/ plant-interest-rate 100))
        network-interest-rate (float (/ network-interest-rate 100))
        
        biomass-price         (float (/ biomass-price 100))
        electricity-in-price  (float (/ electricity-in-price 100))
        electricity-out-price (float (/ electricity-out-price 100))
        gas-price             (float (/ gas-price 100))
        heat-price            (float (/ heat-price 100))
        ]

    (-> output
        (update :infrastructures
                #(map
                  (fn [i] (assoc i
                                 :period (float network-period)
                                 :discountRate network-interest-rate))
                  %))

        (update :processes
                #(map (fn [p]
                        (assoc p
                               :period (float plant-period)
                               :discountRate plant-interest-rate))
                      %))

        (assoc-in [:resflow :tariffs]
                  [{:period "average" :process "heatex" :resource "dist_heat"
                    :value (- (float heat-price))}])

        (update :resources
                #(-> (group-by :varname %)
                     (update-in ["elec" 0]
                                assoc
                                :exportPrice electricity-out-price
                                :importCost electricity-in-price
                                :emissions electricity-emissions)
                     (update-in ["gas" 0]
                                assoc
                                :exportPrice 0
                                :importCost gas-price
                                :emissions gas-emissions)
                     (update-in ["chips" 0]
                                assoc
                                :exportPrice 0
                                :importCost biomass-price
                                :emissions biomass-emissions)
                     (->> (mapcat second))))

        (update :resflow assoc
                :carbonCap (float carbon-cap)
                :ghgWeight (float carbon-cost))

        (update :resources
                #(map (fn [res]
                        (assoc res :maxImportCells number-of-candidates)) %))
        )))

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

(declare add-solution)

(defn solve
  "Solve the INSTANCE, returning an updated instance with solution
  details in it. Probably needs running off the main thread."
  [label config instance]
  
  (let [instance (operations/remove-solution instance) ;; throw away
        ;; any existing
        ;; solution

        working-directory (nio/path (config :solver-directory))

        solver-command (config :solver-command)

        included-candidates (->> (::document/candidates instance)
                                 (vals)
                                 (filter (comp #{:optional :required} ::candidate/inclusion)))

        net-graph (simplify-topology included-candidates)
        net-graph (summarise-attributes net-graph included-candidates)

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
            (float (or (/ (attr/attr net-graph (vec edge) :cost) 1000) 0))
            ]))

        process-locations
        (put-csv
         "process-locations.csv"
         ;; [[id process lb ub]]
         (for [{id ::candidate/id type ::candidate/type} included-candidates
               tech (->> instance
                         ::document/technologies
                         (map ::technology/id)
                         set)
               
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
            (assoc-in [:resflow :processLocations] (rel-path process-locations))
            
            (update :infrastructures
                    #(-> (group-by :varname %)
                         (update-in ["heat_net" 0]
                                    assoc :required (rel-path edge-details))
                         (->> (mapcat second))))
            
            (add-processes-from instance)
            (set-prices-from instance)

            (yaml/generate-string))]
    
    ;; write the scenario down
    (spit (io/file working-directory "scenario.yml") scenario)
    
    ;; invoke the solver
    (let [start-time (System/currentTimeMillis)
          output (sh solver-command
                     "--customdata" "scenario.yml"
                     "--solver" "glpk"
                     "--datadir" "."
                     :dir working-directory)
          end-time (System/currentTimeMillis)

          solution {::solution/runtime (- end-time start-time)
                    ::solution/log (str (:out output) "\n" (:err output))
                    }

          _ (println "Solver ran in" (- end-time start-time) "ms")
          
          instance
          
          (try
            (-> instance
                (assoc ::solution/solution solution)
                (add-solution working-directory net-graph))
            (catch Exception e
              (println e)
              (println (:err output))
              (assoc-in instance [::solution/solution ::solution/status] :error)
              )
            )
          ]
      (spit (io/file working-directory "solved-instance.edn") instance) ;; pprint?
      instance
      ))

  )

(defn- add-solution [instance working-directory net-graph]
  (let [results-json
        (-> (io/file working-directory "results.jsn")
            (slurp)
            (clojure.string/replace #"-?Infinity" "\"$0\"") ;; pyomo produces invalid json
            (json/read-str :key-fn #(-> (.toLowerCase %)
                                        (clojure.string/replace #"[^a-z]+" "-")
                                        (keyword)
                                        )))

        _ (def *last-results* results-json)
        
        termination-condition
        (or (keyword (get-in results-json [:solver 0 :termination-condition]))
            :no-solution)

        objective-value
        (get-in results-json
                [:solution 1 :objective :objfn :value])

        output-file (fn [& parts]
                      (apply io/file working-directory parts))
        
        read-output (fn [name]
                      (with-open [reader (io/reader (output-file "out" name))]
                        (doall (read-csv-map reader))))

        instance (assoc-in instance [::solution/solution ::solution/status]
                           termination-condition)
        ]

    (case termination-condition
      (:feasible :optimal)
       ;; there should be more outputs
      (let [included-candidates
            (->> instance
                 ::document/candidates
                 (vals)
                 (filter (comp #{:required :optional} ::candidate/inclusion)))

            {paths :path supplies :supply demands :demand}
            (group-by ::candidate/type included-candidates)

            supply-ids (set (map ::candidate/id supplies))
            demand-ids (set (map ::candidate/id demands))
            buildings-ids (set/union supply-ids demand-ids)

            network-links (read-output "network.csv")
            processes (read-output "process.csv")
            imports (read-output "import.csv")
            metrics
            (->> (read-output "metrics.csv")
                 (map (fn [row]
                        (update row :value #(Double/parseDouble %)))))

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
                                     (set/intersection buildings-ids))

            supply-technologies (->> processes
                                     (filter (comp supply-ids :cell))
                                     (group-by :cell))

            
            set-supply-tech
            #(assoc-in % [::solution/candidate ::solution/technologies]
                       (get supply-technologies (::candidate/id %)))

            include-candidate #(assoc-in % [::solution/candidate ::solution/included] true)
            
            set-flow #(assoc-in % [::solution/candidate ::solution/heat-flow]
                                (path-flows (::candidate/id %)))
            ]
        (-> instance
            (assoc-in [::solution/solution ::solution/objective-value] objective-value)
            (assoc-in [::solution/solution ::solution/metrics] metrics)
            (operations/map-candidates include-candidate (keys path-flows))
            (operations/map-candidates set-flow (vec (keys path-flows)))
            (operations/map-candidates include-candidate included-vertex-ids)
            (operations/map-candidates set-supply-tech (keys supply-technologies))
            ))

      ;; no solution exists
      instance
      )
    ))
    


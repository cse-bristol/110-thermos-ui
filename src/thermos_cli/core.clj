(ns thermos-cli.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [thermos-importer.geoio :as geoio]
            [thermos-importer.spatial :as spatial]
            [clojure.data.json :as json]
            [cljts.core :as jts]
            [clojure.tools.logging :as log]
            
            [thermos-backend.importer.process :as importer]
            [thermos-backend.solver.interop :as interop]
            [thermos-util :refer [as-double as-boolean as-integer assoc-by]]
            [thermos-util.converter :as converter]
            [thermos-importer.lidar :as lidar]
            
            [thermos-specs.document :as document]
            [thermos-specs.defaults :as defaults]

            [thermos-specs.path :as path]
            [thermos-specs.measure :as measure]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.supply :as supply]
            [thermos-specs.tariff :as tariff]
            [thermos-specs.solution :as solution]
            [mount.core :as mount]
            [clojure.pprint :refer [pprint]]

            [loom.graph :as graph]
            [loom.alg :as graph-alg])
  (:gen-class))

;; THERMOS CLI tools for Net Zero Analysis

(defn- output ^java.io.Closeable [thing]
  (if (= thing "-")
    (proxy [java.io.FilterWriter] [(io/writer System/out)]
      (close [] (proxy-super flush)))
    (io/writer (io/file thing))))

(defn- conj-arg [m k v]
  (update m k conj v))

(def options
  [[nil "--name NAME" "A name to put in the summary output"]
   ["-i" "--base FILES" "The problem to start with - this may contain geometry already.
An efficient way to use the tool is to put back in a file produced by a previous -o output."
    :assoc-fn conj-arg]
   
   ["-o" "--output FILE" "The problem & solution state will be written in here as EDN."]
   ["-s" "--summary-output FILE" "A file where some json summary stats about the problem will go."]
   ["-j" "--json-output FILE" "The geometry data from the final state will be put here as geojson."]
   [nil  "--temp-dir DIR" "Where to put temporary files" :default "/tmp/thermos"]
   [nil  "--preserve-temp" "If not set, temporary files will be removed after run"]
   ["-m" "--map FILE*"
    "Geodata files containing roads or buildings.
These geometries will replace everything in the base scenario (NOT combine with).
If there are roads in there, they will be noded and connected to the buildings.
If there are buildings, they will have demand and peak demand computed, subject to the other fields configured."
    :assoc-fn conj-arg]
   ["-l" "--lidar FILE" "Path to some LIDAR - if map data is given, the LIDAR will be used to put heights on it."
    :assoc-fn conj-arg]
   [nil "--degree-days NUMBER" "Annual heating degree days (relative to 17°) to use for running heat demand model."
    :parse-fn #(Double/parseDouble %)
    :default (* 0.813 2501.0)]
   [nil "--connect-to-connectors" "Allow connecting to connectors."]
   [nil "--shortest-face LENGTH" "When finding face centers, only faces longer than this will be considered."
    :default 3.0
    :parse-fn #(Double/parseDouble %)]
   [nil "--solve" "Run the network model solver."]
   [nil "--height-field FIELD" "A height field, used in preference to LIDAR."]
   [nil "--supply-field FIELD" "A field which, if true, will be used to select a supply location."]
   [nil "--fallback-height-field FIELD" "A height field, used if LIDAR (and given height) is missing."]
   [nil "--resi-field FIELD" "A field which is used to tell the demand model if a building is residential."]
   [nil "--demand-field FIELD" "A kwh/yr field. If given, this will be used in preference to the demand model."]
   [nil "--confidence-field FIELD" "How confident you are in the demand field (max / use).
If given, then a shape for which this says 'max' will get the maximum of demand modelled and demand-field value.
If it says 'use', then the demand-field value will be used (unless it's not numeric)."]
   [nil "--peak-field FIELD" "A kwp field. If given, this will be used in preference to peak model."]
   [nil "--count-field FIELD" "Connection count. Otherwise we assume 1."]
   [nil "--require-all" "Require all buildings be connected to network."]
   ["-f" "--preserve-field FIELD*" "A field to preserve from input data (e.g. TOID).
If the scenario definition refers to some fields, you mention them here or they will be stripped out before the scenario is applied."
    :assoc-fn conj-arg]

   [nil "--output-predictors FILE"
    "Write out the things which went into the demand prediction method"]
   
   [nil "--insulation FILE*"
    "A file containing insulation definitions"
    :assoc-fn conj-arg]
   [nil "--alternatives FILE*"
    "A file containing alternative technologies"
    :assoc-fn conj-arg]
   [nil "--supply FILE*"
    "A file containing supply parameters for supply point"]
   [nil "--pipe-costs FILE"
    "A file containing pipe cost parameters. Complex structure for this means not mergeable."]
   [nil "--tariffs FILE*"
    "A file containing tariff definitions"
    :assoc-fn conj-arg]
   [nil "--top-n-supplies N" "Number of supplies to introduce into the map by taking the top N demands"
    :default 1
    :parse-fn #(Integer/parseInt %)]

   [nil "--max-runtime N" "Max runtime hours"
    :parse-fn #(Double/parseDouble %)]
   [nil "--mip-gap G" "Mip gap proportion"
    :parse-fn #(Double/parseDouble %)]
   
   [nil "--set '[A B C V]'"
    "Set the value at path A B C to value V"
    :parse-fn #(edn/read-string %)
    :assoc-fn conj-arg]
   
   ["-h" "--help" "me Obi-Wan Kenobi - You're my only hope."]])

(defmethod print-method org.locationtech.jts.geom.Geometry
  [this w]
  (.write w "#geojson ")
  (print-method (jts/geom->json this) w))

(defn read-edn [file]
  (when file
    (with-open [r (java.io.PushbackReader. (io/reader file))]
      (edn/read
       {:readers {'geojson jts/json->geom}}
       r))))

(defn- concat-edn [files]
  (when files
    (doall
     (apply concat (map read-edn files)))))

(defn- node-connect
  "Given a set of shapes, do the noding and connecting dance"
  [{crs ::geoio/crs features ::geoio/features}

   connect-to-connectors
   shortest-face-length]
  (when (seq features)
    (let [is-line (comp boolean #{:line-string :multi-line-string} ::geoio/type)
          
          {lines true not-lines false}
          (group-by is-line features)

          lines (spatial/node-paths lines :snap-tolerance 0.01 :crs crs)

          [buildings roads]
          (if lines
            (spatial/add-connections
             crs not-lines lines
             :shortest-face-length shortest-face-length
             :connect-to-connectors false)
            [not-lines nil])]

      [(spatial/trim-dangling-paths roads buildings 1.0)
       buildings])))

(defn- generate-demands [buildings

                         {lidar :lidar
                          degree-days :degree-days

                          resi-field :resi-field
                          height-field :height-field
                          fallback-height-field :fallback-height-field
                          peak-field :peak-field
                          demand-field :demand-field
                          count-field :count-field
                          confidence-field :confidence-field}
                         ]
  
  (when (seq buildings)
    (let [sqrt-degree-days (Math/sqrt degree-days)
          lidar-index (->> lidar
                           (map io/file)
                           (mapcat file-seq)
                           (filter #(and (.isFile %)
                                         (let [name (.getName %)]
                                           (or (.endsWith name ".tif")
                                               (.endsWith name ".tiff")))))
                           (lidar/rasters->index))

          buildings (lidar/add-lidar-to-shapes buildings lidar-index)

          buildings (::geoio/features buildings)
          ]
      ;; run the model for each building
      (for [b buildings]
        (let [is-resi (as-boolean (or
                                   (not resi-field)
                                   (and resi-field
                                        (get b resi-field))))
              height (and height-field
                          (as-double (get b height-field)))

              fallback-height (and fallback-height-field
                                   (as-double (get b fallback-height-field)))
              
              peak (and peak-field
                        (as-double (get b peak-field)))

              demand (and demand-field
                          (as-double (get b demand-field)))

              confidence (and confidence-field
                              (#{:use :max} (get b confidence-field)))
              
              count (or (and count-field
                             (as-integer (get b count-field)))
                        1)]
          
          (-> b
              (assoc :residential is-resi
                     :connection-count count)
              (cond->
                height
                (assoc :height height)

                fallback-height
                (assoc :fallback-height fallback-height)

                demand
                (assoc :annual-demand demand)

                (and confidence demand)
                (assoc :use-annual-demand (keyword confidence)))

              (importer/produce-heat-demand sqrt-degree-days)
              (as-> x
                  (assoc x :peak-demand
                         (or peak
                             (importer/run-peak-model
                              (:annual-demand x)))))))))))

(defn- match
  "Match ITEM, a map, against OPTIONS, a list of things with a :rule in them.
  A :rule is a tuple going [field pattern], so when we (get field
  item) it matches pattern (a regex literal or set of values)"
  [item options & {:keys [match] :or {match :thermos-cli/rule}}]
  
  (and item
       (filter
        (fn matches? [option]
          (let [rule (get option match)]
            (cond
              (= true rule)
              option

              (vector? rule)
              (let [[field pattern] rule
                    field-value (get item field)]
                (cond
                  (string? pattern)
                  (re-find (re-pattern pattern) (str field-value))
                  
                  (set? pattern)
                  (contains? pattern field-value)

                  :else false)
                ))))
        options)))

(defn- add-civils
  "Use `match` to join pipe-costs to paths.
  The rule structure for paths is that the pipe-costs blob contains
  :thermos-cli/rule, which is a series of vectors, that look like

  [civil-cost-id RULE]

  where RULE is like what `match` takes.
  "
  [path pipe-costs]
  (let [civils (:civils pipe-costs)
        rules  (:thermos-cli/rule pipe-costs)]
    (let [civil (first (match path rules :match second))]
      (cond-> path
        civil
        (assoc ::path/civil-cost-id (first civil))))))

(defn- add-insulation [building insulation]
  (let [insulation (match building insulation)
        insulation (set (map ::measure/id insulation))]
    (assoc building ::demand/insulation insulation)))

(defn- add-alternatives [building alternatives]
  (let [alts (match building alternatives)
        counter (::supply/id (first (match building alternatives :match :thermos-cli/counterfactual-rule)))
        alts (set (map ::supply/id alts))
        alts (cond-> alts counter (disj counter))]
    (cond-> building
      (seq alts)
      (assoc ::demand/alternatives alts)

      counter
      (assoc ::demand/counterfactual counter))))

(defn- add-tariff [building tariffs]
  (let [tariff (::tariff/id (first (match building tariffs)))]
    (cond-> building
      tariff (assoc ::tariff/id tariff))))

(defn- select-top-n-supplies [instance supply top-n]
  (let [{buildings :building paths :path}
        (document/candidates-by-type instance)

        graph
        (interop/create-graph buildings paths)

        components
        (graph-alg/connected-components graph)

        candidates (::document/candidates instance)
        
        ranked-building-ids
        (fn [building-ids]
          (let [buildings
                (keep
                 (fn [id]
                   (let [candidate (get candidates id)]
                     (when (candidate/is-building? candidate)
                       {:id (::candidate/id candidate)
                        :kwh (::demand/kwh candidate)
                        :centroid (.getCentroid (::candidate/geometry candidate))})))
                 building-ids)

                buildings
                (for [{id :id here :centroid} buildings]
                  {:id id
                   :value
                   (double
                    (reduce
                     +
                     (for [{kwh :kwh there :centroid} buildings]
                       (/ kwh
                          (+ 50.0  ;; might work?
                             (jts/geodesic-distance
                              (.getCoordinate here)
                              (.getCoordinate there))
                             )))))})
                ]

            (->> buildings
                 (sort-by :value #(compare %2 %1))
                 (keep :id))))

        winning-ids
        (mapcat
         #(take top-n (ranked-building-ids %))
         components)
        ]
    (log/info "Adding" (count winning-ids) "supply locations to" (count components) "components")
    (cond-> instance
      (and (seq winning-ids) supply)
      (document/map-candidates
       #(merge supply %)
       winning-ids))
    ))

(defn- select-input-supplies [instance supply supply-field]
  (log/info "Adding supplies based on" supply-field)
  (document/map-buildings
   instance
   (fn [building]
     (if-let [field (as-boolean (get building supply-field))]
       (merge supply building)
       building))))

(defn- select-supply-location [instance options]
  (cond

    (:supply-field options)
    (select-input-supplies instance (:supply options) (:supply-field options))

    (pos? (:top-n-supplies options))
    (select-top-n-supplies instance (:supply options) (:top-n-supplies options))

    :else ;; NOP
    instance))

(defn- make-candidates [paths buildings preserve-fields]
  (let [paths (for [path paths]
                (-> path
                    (merge {::candidate/id        (::geoio/id path)
                            ::candidate/type      :path
                            ::candidate/subtype   (:subtype path)
                            ::candidate/inclusion :optional
                            ::path/length         (or (::spatial/length path) 0)
                            ::path/start          (::geoio/id (::spatial/start-node path))
                            ::path/end            (::geoio/id (::spatial/end-node path))
                            ::candidate/geometry  (::geoio/geometry path)})))
        buildings (for [building buildings]
                    (-> building
                        (merge {::candidate/id            (::geoio/id building)
                                ::candidate/subtype       (:subtype building)
                                ::candidate/type          :building
                                ::candidate/inclusion     :optional
                                ::candidate/wall-area     (:wall-area building)
                                ::candidate/roof-area     (:roof-area building)
                                ::candidate/ground-area   (:ground-area building)
                                ::candidate/connections   (::spatial/connects-to-node building)
                                ::candidate/geometry      (::geoio/geometry building)
                                
                                ::demand/kwh              (:annual-demand building)
                                ::demand/kwp              (:peak-demand building)
                                ::demand/connection-count (:connection-count building)
                                })))]

    (assoc-by
     (concat paths buildings)
     ::candidate/id)))

(defn- sum-costs [costs]
  (apply merge-with +
         (for [c costs :when c]
           {:present (:present c 0)
            :total (:total c 0)
            :annual (:annual c 0)
            :principal (:principal c 0)})))

(defn- problem-summary
  "Compute some useful summary stats about the given instance."
  [instance name]

  (let [{buildings :building paths :path}
        (group-by ::candidate/type
                  (vals (::document/candidates instance)))

        {network-buildings true
         individual-buildings false}
        (group-by (comp boolean candidate/is-connected?)
                  buildings)

        supplies (filter ::solution/capacity-kw buildings)

        network-paths
        (filter candidate/is-connected? paths)
        ]
    {:name name

     :problem
     {:building-count (count buildings)
      :address-count  (reduce + (keep ::demand/connection-count buildings))
      :path-count     (count paths)
      :kwh            (reduce + (keep ::demand/kwh buildings))
      :kwp            (reduce + (keep ::demand/kwp buildings))
      :path-length   (reduce + (keep ::path/length paths))

      :components          (count (keep ::supply/capacity-kwp buildings))
      }

     :solution
     {:runtime             (::solution/runtime instance)
      :gap                 (::solution/gap instance)
      :objective           (::solution/objective instance) }
     
     :network
     
     (let [heat-output (reduce + (keep ::solution/kwh network-buildings))
           heat-input  (->> supplies
                            (keep ::solution/output-kwh)
                            (reduce +))
           ]

       {:building-count    (count network-buildings)
        :address-count     (reduce + (keep ::demand/connection-count network-buildings))

        :undiverse-kwp     (reduce + (keep ::demand/kwp network-buildings))
        :kwp               (reduce + (keep ::solution/capacity-kw supplies))
        :length            (reduce + (keep ::path/length network-paths))

        :kwh-output        heat-output
        :kwh-input         heat-input
        :kwh-lost          (- heat-input heat-output)

        :supply-capex (sum-costs (keep ::solution/supply-capex supplies))
        :heat-cost (sum-costs (keep ::solution/heat-cost supplies))
        :supply-opex (sum-costs (keep ::solution/supply-opex supplies))
        :path-capex (sum-costs (keep ::solution/pipe-capex network-paths))
        :connection-capex (sum-costs (keep ::solution/connection-capex buildings))

        :capex
        (sum-costs (concat
                    (keep ::solution/connection-capex buildings)
                    (keep ::solution/supply-capex buildings)
                    (keep ::solution/pipe-capex network-paths)))

        :opex
        (sum-costs
         (concat
          (keep ::solution/supply-opex buildings)
          (keep ::solution/heat-cost buildings)))
        
        })

     :insulation
     (->>
      (for [[name installed]
            (->> buildings
                 (mapcat ::solution/insulation)
                 (group-by ::measure/name))]
        [name
         {:kwh-avoided    (reduce + (keep :kwh installed))
          :building-count (count installed)
          :area           (reduce + (keep :area installed))
          :capex          (sum-costs installed)}])
      (into {}))
     
     
     :alternatives
     (->>
      (for [[name alts]
            (group-by (comp ::supply/name ::solution/alternative)
                      individual-buildings)]
        [(or name "Nothing at all")
         {:kwh-output (reduce + (keep ::solution/kwh alts))
          :kwp        (reduce + (keep ::demand/kwp alts))
          :building-count (count alts)
          :address-count  (reduce + (keep ::demand/connection-count alts))
          :capex (sum-costs (keep (comp :capex ::solution/alternative) alts))
          :heat-cost (sum-costs (keep (comp :heat-cost ::solution/alternative) alts))
          :opex (sum-costs (keep (comp :opex ::solution/alternative) alts))}])
      (into {}))}))

(defn- write-geojson [data writer]
  (json/write
   data
   writer
   :value-fn
   (fn write-geojson-nicely [k v]
     (if (instance? org.locationtech.jts.geom.Geometry v)
       (jts/geom->json v)
       v))))

(defn- set-values [instance things-to-set]
  (reduce
   (fn [i s]
     (let [path (take (dec (count s)) s)
           value (last s)]
       (log/info "change" path "from"
                 (get-in i path)
                 "to"
                 value)
       (assoc-in i path value)))
   instance
   things-to-set))

(defn- require-all-buildings [instance]
  (document/map-buildings
   instance
   (fn [building] (assoc building ::candidate/inclusion :required))))

(defn --main [options]
  (mount/start-with {#'thermos-backend.config/config
                     {:solver-directory (:temp-dir options)}})
  (let [output-path       (:output options)
        summary-output-path (:summary-output options)

        output-predictors-path (:output-predictors options)
        
        json-path         (:json-output options)
        
        geodata           (when (seq (:map options))
                            (geoio/read-from-multiple (:map options)
                                                      :key-transform identity))

        [paths buildings] (node-connect geodata
                                        (:connect-to-connectors options)
                                        (:shortest-face options))
        
        buildings         (when (seq buildings)
                            (-> {::geoio/features buildings
                                 ::geoio/crs (::geoio/crs geodata)}

                                (generate-demands    options)
                                
                                ;; areas for measures to work on
                                (->> (map importer/add-areas))))
        
        instance          (apply merge defaults/default-document
                                 (when-let [base
                                            (seq
                                             (filter
                                              #(let [e (.exists (io/file %))]
                                                 (when-not e
                                                   (log/warn "--base" % "doesn't exist"))
                                                 e)
                                              (:base options)))]
                                   (doall (map read-edn (reverse base)))))

        required-fields   (:preserve-field options)

        saying            (fn [x s] (log/info s) x)
        
        instance          (cond-> instance
                            (or (seq paths) (seq buildings))
                            (-> (saying "Replace geometry")
                                (assoc  ::document/candidates   (make-candidates paths buildings required-fields)))
                            
                            (seq (:tariffs options))
                            (-> (saying "Replace tariffs")
                                (assoc ::document/tariffs (assoc-by (:tariffs options) ::tariff/id))
                                (document/map-buildings (let [tariffs (:tariffs options)] #(add-tariff % tariffs))))

                            (seq (:pipe-costs options))
                            (-> (saying "Replace pipe costs")
                                (assoc ::document/pipe-costs (:pipe-costs options))
                                (document/map-paths
                                 (let [pipe-costs (:pipe-costs options)]
                                   #(add-civils % pipe-costs))))

                            (seq (:insulation options))
                            (-> (saying "Replace insulation")
                                (assoc  ::document/insulation   (assoc-by (:insulation options) ::measure/id))
                                (document/map-buildings (let [insulation (:insulation options)] #(add-insulation % insulation))))
                            
                            (seq (:alternatives options))
                            (-> (saying "Replace alts")
                                (assoc  ::document/alternatives (assoc-by (:alternatives options) ::supply/id))
                                (document/map-buildings (let [alternatives (:alternatives options)] #(add-alternatives % alternatives))))


                            (seq (:supply options))
                            (-> (saying "Add supplies")
                                (select-supply-location options))

                            (seq (:set options))
                            (set-values (:set options))

                            (:max-runtime options)
                            (assoc :thermos-specs.document/maximum-runtime
                                   (:max-runtime options))

                            (:mip-gap options)
                            (assoc :thermos-specs.document/mip-gap
                                   (:mip-gap options))

                            (:require-all options)
                            (-> (saying "Requiring all buildings")
                                (require-all-buildings))
                            
                            (:solve options)
                            (-> (saying "Solve")
                                (as-> instance
                                    (interop/solve "" instance
                                                   :remove-temporary-files
                                                   (not (:preserve-temp options)))
                                  )))
        ]

    (when output-predictors-path
      (log/info "Saving predictors to" output-predictors-path)
      (with-open [w (output output-predictors-path)]
        (write-geojson
         {:type :FeatureCollection
          :features
          (for [b buildings]
            {:type :Feature
             :id (::geoio/id b)
             :properties (dissoc b ::geoio/geometry)
             :geometry (::geoio/geometry b)})}
         w)))
    
    (when json-path
      (log/info "Saving geojson to" json-path)
      (with-open [w (output json-path)]
        (-> instance
            (converter/network-problem->geojson)
            (write-geojson w))))
    
    (when output-path
      (log/info "Saving edn to" output-path)
      (with-open [w (output output-path)]
        (if (= output-path "-") 
          (pprint instance w)
          (binding [*out* w] (prn instance)))))

    (when summary-output-path
      (log/info "Saving summary to" summary-output-path)
      (with-open [w (output summary-output-path)]
        (json/write (problem-summary instance (:name options)) w))))
  
  (mount/stop))

(defn- generate-ids [things id]
  (map-indexed (fn [i t] (assoc t id i)) things))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args options)

        options (-> options
                    (update :insulation   concat-edn)
                    (update :alternatives concat-edn)
                    (update :tariffs      concat-edn)
                    (update :pipe-costs   read-edn)
                    (update :supply       read-edn)
                    
                    (update :insulation   generate-ids ::measure/id)
                    (update :alternatives generate-ids ::supply/id)
                    (update :tariffs      generate-ids ::tariff/id)
                    )]
    (cond
      (:help options)
      (do
        (println "SUMMARY:")
        (println summary))

      :else
      (--main options)
      )
    ))





(comment
  (-main "-m" "/home/hinton/p/110-thermos/network-model-validation/data/scenario-A60.gpkg"
         "-j" "/home/hinton/p/110-thermos/network-model-validation/A60.gjson"
         "--supply-field" "SupplyDema"
         "--supply" "/home/hinton/p/110-thermos/network-model-validation/supply.edn"
         "--tariffs" "/home/hinton/p/110-thermos/network-model-validation/tariffs.edn"
         "--demand-field" "kWh"
         "--require-all"
         "--solve"
         "--summary-output" "-"
         )
  )

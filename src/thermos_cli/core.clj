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
            )
  (:gen-class))

;; THERMOS CLI tools for Net Zero Analysis

(defn- conj-arg [m k v]
  (update m k conj v))

(def options
  [["-i" "--base FILE" "The problem to start with - this may contain geometry already.
An efficient way to use the tool is to put back in a file produced by a previous -o output."]
   ["-o" "--output FILE" "The problem & solution state will be written in here as EDN."]
   ["-s" "--summary-output FILE" "A file where some json summary stats about the problem will go."]
   ["-j" "--json-output FILE" "The geometry data from the final state will be put here as geojson."]
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
    :default 2501.0]
   [nil "--connect-to-connectors" "Allow connecting to connectors."]
   [nil "--shortest-face LENGTH" "When finding face centers, only faces longer than this will be considered."
    :default 3.0
    :parse-fn #(Double/parseDouble %)]
   [nil "--solver PATH" "Path to the solver program; if given, runs solver."]
   [nil "--height-field FIELD" "A height field, used in preference to LIDAR."]
   [nil "--fallback-height-field FIELD" "A height field, used if LIDAR (and given height) is missing."]
   [nil "--resi-field FIELD" "A field which is used to tell the demand model if a building is residential."]
   [nil "--demand-field FIELD" "A kwh/yr field. If given, this will be used in preference to the demand model."]
   [nil "--confidence-field FIELD" "How confident you are in the demand field (max / use).
If given, then a shape for which this says 'max' will get the maximum of demand modelled and demand-field value.
If it says 'use', then the demand-field value will be used (unless it's not numeric)."]
   [nil "--peak-field FIELD" "A kwp field. If given, this will be used in preference to peak model."]
   [nil "--count-field FIELD" "Connection count. Otherwise we assume 1."]
   ["-f" "--preserve-field FIELD*" "A field to preserve from input data (e.g. TOID).
If the scenario definition refers to some fields, you mention them here or they will be stripped out before the scenario is applied."
    :assoc-fn conj-arg]
   [nil "--insulation FILE*"
    "A file containing insulation definitions"
    :assoc-fn conj-arg]
   [nil "--alternatives FILE*"
    "A file containing alternative technologies"
    :assoc-fn conj-arg]
   [nil "--supply FILE*"
    "A file containing supply parameters for supply point"]
   [nil "--civils FILE*"
    "A file containing civil cost parameters"
    :assoc-fn conj-arg]
   [nil "--tariffs FILE*"
    "A file containing tariff definitions"
    :assoc-fn conj-arg]
   [nil "--top-n-supplies N" "Number of supplies to introduce into the map by taking the top N demands"
    :default 1
    :parse-fn #(Integer/parseInt %)]
   
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

          lines (spatial/node-paths lines)

          [buildings roads]
          (if lines
            (spatial/add-connections
             crs not-lines lines
             :shortest-face-length shortest-face-length
             :connect-to-connectors false)
            [not-lines nil])]

      [roads buildings])))

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

              (importer/produce-demand sqrt-degree-days)
              (as-> x
                  (assoc x :peak-demand
                         (or peak
                             (importer/run-peak-model
                              (:annual-demand x)))))))))))

(defn- match
  "Match ITEM, a map, against OPTIONS, a list of things with a :rule in them.
  A :rule is a tuple going [field pattern], so when we (get field item) it matches pattern (a regex literal)"
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

(defn- add-civils [path civils]
  (let [civil (first (match path civils))]
    (cond-> path
      civil
      (assoc ::path/civil-cost-id (::path/civil-cost-id civil)))))

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

(defn- select-supply-location [instance supply top-n]
  ;; for now we find the N largest annual demands
  ;; we could do a spatial rule as well / instead?
  (let [biggest-demands
        (take top-n
              (sort-by ::demand/kwh #(compare %2 %1)
                       (vals (::document/candidates instance))))]
    (log/info "Adding" (count biggest-demands) "supply locations")
    (cond-> instance
      (and (seq biggest-demands) supply)
      (document/map-candidates
       #(merge supply %)
       (map ::candidate/id biggest-demands)))))

(defn- make-candidates [paths buildings preserve-fields]
  (let [paths (for [path paths]
                (-> path
                    (select-keys preserve-fields)
                    (merge {::candidate/id        (::geoio/id path)
                            ::candidate/type      :path
                            ::candidate/inclusion :optional
                            ::path/length         (or (::spatial/length path) 0)
                            ::path/start          (::geoio/id (::spatial/start-node path))
                            ::path/end            (::geoio/id (::spatial/end-node path))
                            ::candidate/geometry  (::geoio/geometry path)})))
        buildings (for [building buildings]
                    (-> building
                        
                        (select-keys (concat [:demand-source] preserve-fields))
                        
                        (merge {::candidate/id            (::geoio/id building)
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
  [instance]

  (let [{buildings :building paths :path}
        (group-by ::candidate/type
                  (vals (::document/candidates instance)))

        {network-buildings true
         individual-buildings false}
        (group-by (comp boolean candidate/is-connected?)
                  buildings)

        network-paths
        (filter candidate/is-connected? paths)
        ]
    {:number-of-buildings (count buildings)
     :number-of-paths     (count paths)

     :network
     {:building-count    (count network-buildings)
      :address-count     (reduce + (keep ::demand/connection-count network-buildings))
      :total-demand      (reduce + (keep ::solution/kwh network-buildings))
      :total-peak        (reduce + (keep ::demand/kwp network-buildings))
      :supply-capacity   (reduce + (keep ::solution/capacity-kw network-buildings))
      :network-length    (reduce + (keep ::path/length network-paths))

      :supply-output-kwh (->> network-buildings
                              (keep ::solution/output-kwh)
                              (reduce +))

      :supply-capex (sum-costs (keep ::solution/supply-capex buildings))
      :supply-heat-cost (sum-costs (keep ::solution/heat-cost buildings))
      :supply-opex (sum-costs (keep ::solution/supply-opex buildings))
      :path-capex (sum-costs (keep ::solution/pipe-capex network-paths))
      }

     :insulation
     (->>
      (for [[name installed]
            (->> buildings
                 (mapcat ::solution/insulation)
                 (group-by ::measure/name))]
        [name
         {:kwh   (reduce + (keep :kwh installed))
          :area  (reduce + (keep :kwh installed))
          :capex (sum-costs installed)}])
      (into {}))
     
     
     :alternatives
     (->>
      (for [[name alts]
            (group-by (comp ::supply/name ::solution/alternative)
                      individual-buildings)]
        [(or name "Nothing at all")
         {:kwh (reduce + (keep ::solution/kwh alts))
          :kwp (reduce + (keep ::demand/kwp alts))
          :building-count (count alts)
          :address-count  (reduce + (keep ::demand/connection-count alts))
          :capex (sum-costs (keep (comp :capex ::solution/alternative) alts))
          :heat-cost (sum-costs (keep (comp :heat-cost ::solution/alternative) alts))
          :opex (sum-costs (keep (comp :opex ::solution/alternative) alts))}])
      (into {}))}))

(defn --main [options]
  (mount/start-with {#'thermos-backend.config/config
                     {:solver-directory "."
                      :solver-command (:solver options)}})
  (let [output-path       (:output options)
        summary-output-path (:summary-output options)
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
        
        instance          (if-let [base (:base options)]
                            (read-edn base)
                            defaults/default-document)

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

                            (seq (:civils options))
                            (-> (saying "Replace civils")
                                (assoc ::document/civil-costs  (assoc-by (:civils options) ::path/civil-cost-id))
                                (document/map-paths (let [civils (:civils options)] #(add-civils % civils))))

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
                                (select-supply-location (:supply options)
                                                        (:top-n-supplies options)))

                            (:solver options)
                            (-> (saying "Solve")
                                (->> (interop/solve "job-"))))
        ]
    (when json-path
      (log/info "Saving geojson to" json-path)
      (with-open [w (io/writer (io/file json-path))]
        (-> instance
            (converter/network-problem->geojson)
            (json/write w
                        :value-fn
                        (fn write-geojson-nicely [k v]
                          (if (instance? org.locationtech.jts.geom.Geometry v)
                            (jts/geom->json v)
                            v)
)
                        ))))
    
    (when output-path
      (log/info "Saving edn to" output-path)
      (if (= output-path "-")
        (pprint instance)

        (with-open [w (io/writer (io/file output-path))]
          (pprint instance w))))

    (when summary-output-path
      (log/info "Saving summary to" summary-output-path)
      (let [summary (problem-summary instance)]
        (pprint summary)
        (with-open [w (io/writer (io/file summary-output-path))]
          (json/write summary w)))))
  
  (mount/stop))

(defn- generate-ids [things id]
  (map-indexed (fn [i t] (assoc t id i)) things))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args options)

        options (-> options
                    (update :insulation   concat-edn)
                    (update :alternatives concat-edn)
                    (update :tariffs      concat-edn)
                    (update :civils       concat-edn)
                    (update :supply       read-edn)

                    (update :insulation   generate-ids ::measure/id)
                    (update :alternatives generate-ids ::supply/id)
                    (update :tariffs      generate-ids ::tariff/id)
                    (update :civils       generate-ids ::path/civil-cost-id)
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





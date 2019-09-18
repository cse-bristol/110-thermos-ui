(ns thermos-cli.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [thermos-importer.geoio :as geoio]
            [thermos-importer.spatial :as spatial]
            [clojure.data.json :as json]
            
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
  [["-i" "--base FILE" "The base problem to merge stuff into"]
   ["-o" "--output FILE" "Where to put result"]
   ["-j" "--json-output FILE" "Where to put result as geojson"]
   ["-m" "--map FILE*"
    "Geodata files containing roads or buildings"
    :assoc-fn conj-arg]
   ["-l" "--lidar FILE" "LIDAR inputs"
    :assoc-fn conj-arg]
   [nil "--degree-days NUMBER" "Degree days (relative to 17°)"
    :parse-fn #(Double/parseDouble %)
    :default 2501.0]
   [nil "--connect-to-connectors" "Whether to allow connecting to connectors"]
   [nil "--shortest-face LENGTH" "When finding face centers, shortest face length"
    :default 3.0
    :parse-fn #(Double/parseDouble %)]
   [nil "--solver PATH" "Path to the solver program; if given, runs solver."]
   [nil "--height-field FIELD" "A height field"]
   [nil "--resi-field FIELD" "A resi field"]
   [nil "--demand-field FIELD" "A kwh/yr field"]
   [nil "--peak-field FIELD" "A kwp field"]
   [nil "--count-field FIELD" "Connection count"]
   ["-f" "--preserve-field FIELD*" "A field to preserve from input data (e.g. TOID)"
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

(defn read-edn [file]
  (when file
    (with-open [r (java.io.PushbackReader. (io/reader file))]
      (edn/read r))))

(defn- concat-edn [files]
  (when files
    (doall
     (apply concat (map read-edn files))
     )))

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
          (spatial/add-connections
           crs not-lines lines
           :shortest-face-length shortest-face-length
           :connect-to-connectors false)]

      [roads buildings])))

(defn- generate-demands [buildings

                         degree-days lidar

                         resi-field
                         height-field
                         peak-field
                         demand-field
                         count-field
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
                                        (get resi-field b))))
              height (and height-field
                          (as-double (get height-field b)))
              peak (and peak-field
                        (as-double (get peak-field b)))
              demand (and demand-field
                          (as-double (get demand-field b)))

              count (or (and count-field
                             (as-integer (get count-field b)))
                        1)
              
              ]
          (-> b
              (assoc :residential is-resi
                     :connection-count count)
              (cond->
                  height
                (assoc :height height)

                demand
                (assoc :annual-demand demand))
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

(defn- add-civils [paths civils]
  (for [path paths]
    (let [civil (first (match path civils))]
      (cond-> path
        civil
        (assoc ::path/civil-cost-id (::path/civil-cost-id civil))))))

(defn- add-insulation [buildings insulation]
  (for [building buildings]
    (let [insulation (match building insulation)
          insulation (set (map ::measure/id insulation))]
      (assoc building ::demand/insulation insulation))))

(defn- add-alternatives [buildings alternatives]
  ;; need to find counterfactual match
  (for [building buildings]
    (let [alts (match building alternatives)
          counter (::supply/id (first (match building alternatives :match :thermos-cli/counterfactual-rule)))
          alts (set (map ::supply/id alts))
          alts (cond-> alts counter (disj counter))
          ]
      (cond-> building
        (seq alts)
        (assoc ::demand/alternatives alts)

        counter
        (assoc ::demand/counterfactual counter)))))

(defn- add-tariffs [buildings tariffs]
  (for [building buildings]
    (let [tariff (::tariff/id (first (match building tariffs)))]
      (cond-> building
        tariff (assoc ::tariff/id tariff)))))

(defn- select-supply-location [instance supply top-n]
  ;; for now we find the N largest annual demands
  ;; we could do a spatial rule as well / instead?
  (let [biggest-demands
        (take top-n
              (sort-by ::demand/kwh #(compare %2 %1)
                       (vals (::document/candidates instance))))]
    (println "Adding " (count biggest-demands) " supply locations")
    (cond-> instance
      (and (seq biggest-demands) supply)
      (document/map-candidates
       #(merge supply %)
       (map ::candidate/id biggest-demands)))))

(defn- make-candidates [paths buildings preserve-fields]
  (let [paths (for [path paths]
                (-> path
                    (select-keys (concat [::path/civil-cost-id]
                                         preserve-fields))
                    (merge {::candidate/id        (::geoio/id path)
                            ::candidate/type      :path
                            ::candidate/inclusion :optional
                            ::path/length         (or (::spatial/length path) 0)
                            ::path/start          (::geoio/id (::spatial/start-node path))
                            ::path/end            (::geoio/id (::spatial/end-node path))
                            ::candidate/geometry  (::geoio/geometry path)})))
        buildings (for [building buildings]
                    (-> building
                        
                        (select-keys (concat [::tariff/id
                                              :demand-source
                                              ::demand/counterfactual
                                              ::demand/alternatives
                                              ::demand/insulation]
                                             preserve-fields))
                        
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


(defn --main [options]
  (mount/start-with {#'thermos-backend.config/config
                     {:solver-directory "."
                      :solver-command (:solver options)}})
  (let [output-path       (:output options)
        json-path         (:json-output options)
        
        geodata           (geoio/read-from-multiple (:map options)
                                                    :key-transform identity)

        [paths buildings] (node-connect geodata
                                        (:connect-to-connectors options)
                                        (:shortest-face options))
        
        paths             (add-civils paths (:civils options))

        ;; TODO rearrange this so that we can run it in parts:
        ;; 1. Geospatial / estimating operations
        ;;    Seems like these will always have to remove whatever geometry already exists
        ;; 2. Adding controllable parts (supply locations, technologies)
        ;; 3. Running the model on the problem
        
        buildings         (-> {::geoio/features buildings
                               ::geoio/crs (::geoio/crs geodata)}

                              ;; the above is not very nice and consistent, sorry
                              
                              (generate-demands    (:degree-days options)

                                                   (:lidar options)
                                                   (:resi-field options)
                                                   (:height-field options)
                                                   (:peak-field options)
                                                   (:demand-field options)
                                                   (:count-field options))
                              
                              (add-insulation      (:insulation options))
                              (add-alternatives    (:alternatives options))
                              (add-tariffs         (:tariffs options))

                              ;; areas for measures to work on
                              (->> (map importer/add-areas)))
        
        instance          (if-let [base (:base options)]
                            (read-edn base)
                            defaults/default-document)

        instance          (assoc instance
                                 ::document/tariffs      (assoc-by (:tariffs options) ::tariff/id)
                                 ::document/civil-costs  (assoc-by (:civils options) ::path/civil-cost-id)
                                 ::document/insulation   (assoc-by (:insulation options) ::measure/id)
                                 ::document/alternatives (assoc-by (:alternatives options) ::supply/id)
                                 ::document/candidates   (make-candidates paths buildings (:preserve-field options)))

        instance          (select-supply-location instance
                                                  (:supply options)
                                                  (:top-n-supplies options))
        
        
        ]

    
    
    (let [instance (cond-> instance (:solver options) (->> (interop/solve "job")))]
      (when json-path
        (with-open [w (io/writer (io/file json-path))]
          (-> instance
              (converter/network-problem->geojson)
              (json/write w))))
      
      (when output-path
        (if (= output-path "-")
         (pprint instance)

         (with-open [w (io/writer (io/file output-path))]
           (pprint instance w))))


      (let [{buildings :building
             paths :path}
            (group-by ::candidate/type (vals (::document/candidates instance)))]

        (print "\nSUMMARY\n")

        (println (count buildings) "buildings," (count paths) "paths")
        
        (println "Counterfactuals:")
        (pprint
         (frequencies
          (map (comp
                ::supply/name
                (::document/alternatives instance)
                ::demand/counterfactual)
               buildings)))

        (println "Insulations:")
        (pprint
         (frequencies
          (map (comp
                (partial
                 map
                 (comp ::measure/name
                       (::document/insulation instance)))
                ::demand/insulation) buildings)))

        (println "Alts:")
        (pprint
         (frequencies
          (map (comp
                (partial
                 map
                 (comp ::supply/name
                       (::document/alternatives instance)))
                
                ::demand/alternatives) buildings)))

        (println "Civils:")
        (pprint
         (frequencies
          (map (comp
                ::path/civil-cost-name
                (::document/civil-costs instance)
                ::path/civil-cost-id)
               paths)))

        (println "Demand estimate:")
        (pprint
         (frequencies
          (map :demand-source buildings)))

        (println "Buildings connected:")
        (pprint
         (frequencies
          (map candidate/is-connected? buildings)))

        (println "Supplies:")
        (pprint
         (frequencies
          (map candidate/supply-in-solution? buildings)))

        (println "Individual systems:")
        (pprint
         (frequencies
          (map
           (comp ::supply/name
                 ::solution/alternative)
            buildings)))
        
        (println "Paths connected:")
        (pprint
         (frequencies
          (map candidate/is-connected? paths)))

        )))
  
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
      (println summary)

      :else
      (--main options)
      )
    ))

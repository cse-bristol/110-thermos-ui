(ns thermos-backend.importer.process
  (:require [thermos-backend.util :as util]
            [thermos-backend.config :refer [config]]

            [clojure.set :as set]
            [clojure.java.io :as io]
            [org.tobereplaced.nio.file :as nio]

            [thermos-importer.geoio :as geoio]
            [thermos-importer.overpass :as overpass]
            [thermos-importer.lidar :as lidar]
            [thermos-importer.svm-predict :as svm]
            [thermos-importer.lm-predict :as lm]
            [thermos-importer.spatial :as topo]
            [thermos-importer.util :refer [has-extension file-extension]]
            [thermos-backend.importer.sap :as sap]

            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.data.csv :as csv]
            [thermos-importer.spatial :as spatial]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [thermos-util :refer [as-double as-boolean assoc-by distinct-by annual-kwh->kw]]
            [clojure.pprint :refer [pprint]]
            [cljts.core :as jts]
            [clojure.test :as test])
  
  (:import [org.locationtech.jts.geom
            Envelope
            GeometryFactory
            PrecisionModel]))

(defn- keyword-upcase-uscore [s]
  (keyword (str/replace s "_" "-")))

(defn- clean-tag [t]
  (and t (str/capitalize (str/replace t #"[_-]+" " "))))

(defn- remap-building-type
  "We did have a complicated program which took stuff from a file, but
  this is what it boils down to:

  1. If the building has an amenity tag, use that
  2. If it has a non-yes building tag, use that
  3. If it has a landuse (added by overpass.clj) use that"
  [b]
  (let [landuse  (:landuse b)
        building (:building b)
        amenity  (:amenity b)
        building (when-not (= "yes" building) building)]
    (clean-tag (or amenity building landuse))))

(defn- geojson-bounds [{{[coordinates] "coordinates"} "geometry"}]
  (let [lons (map first coordinates)
        lats (map second coordinates)

        min-lon (reduce min lons)
        max-lon (reduce max lons)
        min-lat (reduce min lats)
        max-lat (reduce max lats)

        ;; TODO this probably fails around the antimeridian, and maybe
        ;; to its left more generally
        ]
    [min-lat min-lon max-lat max-lon]))

(defn- query-osm [state parameters]
  (let [get-roads     (-> parameters :roads :source (= :osm))
        get-buildings (-> parameters :buildings :source (= :osm))

        query-area    (if get-buildings
                        (or
                         (-> parameters :buildings :osm :osm-id)
                         ;; this needs way: or rel: on it

                         ;; bounding box query
                         (-> parameters :buildings :osm :boundary geojson-bounds))

                        ;; we are only getting the roads, so we want
                        ;; to get bounds from the existing geometry

                        (let [box (geoio/bounding-box (:buildings state))]
                          [(.getMinY box)
                           (.getMinX box)
                           
                           (.getMaxY box)
                           (.getMaxX box)]))

        set-building-type  #(assoc % :subtype (remap-building-type %))
        set-road-type      #(assoc % :subtype (clean-tag (:highway %)))
        set-specials       #(cond-> %
                              (= (:osm-id %) "289662492")
                              (assoc :name "Rothballer Towers"))
        
        query-results (overpass/get-geometry query-area
                                             :include-buildings get-buildings
                                             :include-highways get-roads)

        query-results
        (for [r query-results]
          (assoc r :identity (:osm-id r)))
        
        buildings
        (->> query-results
             (filter :building)
             (geoio/explode-multis)
             (filter (comp #{:polygon} ::geoio/type))
             (map set-building-type)
             (map set-specials))

        highways
        (->> query-results
             (filter :highway)
             (filter (comp #{:line-string} ::geoio/type))
             (map set-road-type))]

    (cond-> state
      get-buildings
      (assoc :buildings
             {::geoio/crs "EPSG:4326" ::geoio/features buildings})

      get-roads
      (assoc :roads
             {::geoio/crs "EPSG:4326" ::geoio/features highways}))
    ))

(defn load-lidar-index []
  (when-let [lidar-directory (config :lidar-directory)]
    (->> (file-seq (io/file lidar-directory))
         (filter #(and (.isFile %)
                       (let [name (.getName %)]
                         (or (.endsWith name ".tif")
                             (.endsWith name ".tiff")))))
         (lidar/rasters->index))))

(def residential-subtypes
  #{nil ;; no subtype = resi
    "" ;; blank string = resi
    "Residential"
    "House"
    "Apartments"
    "Detached"
    "Dormitory"
    "Terrace"
    "Houseboat"
    "Bungalow"
    "Static caravan"
    "Cabin"})

(def svm-range-limit 1.5)

(defn- load-svm [name]
  (-> name
      (io/resource)
      (slurp)
      (json/read-str :key-fn keyword-upcase-uscore)
      (svm/predictor)))

(defn- load-lm [name]
  (-> name
      (io/resource)
      (slurp)
      (json/read-str :key-fn keyword-upcase-uscore)
      (lm/predictor)))

(def svm-space-3d (load-svm "thermos_backend/importer/space-svm-3d.json"))
(def svm-space-2d (load-svm "thermos_backend/importer/space-svm-2d.json"))

(def lm-space-3d (load-lm "thermos_backend/importer/space-lm-3d.json"))
(def lm-space-2d (load-lm "thermos_backend/importer/space-lm-2d.json"))

(defn- run-svm-models [f sqrt-degree-days]
  (let [x (->> (for [[k v] f
                     :when (and (keyword? k)
                                (or (= :residential k)
                                    (and v (= "thermos-importer.lidar" (namespace k)))))]
                 [(keyword (name k))
                  (if (= :residential k) (boolean v) v)])
               (into {}))

        space-svm-3 (svm-space-3d x)
        svm-result  (or space-svm-3 (svm-space-2d x))
        sap-water (sap/hot-water (:floor-area x))]
    
    (or
     (when (and svm-result
                (> (aget svm-result 1) 1.5))
      (let [lm-value ((if space-svm-3 lm-space-3d lm-space-2d) x)]
        (when (lm-value (>= lm-value 7692.0))
          {:annual-demand (/ lm-value 0.65)
           :sap-water-demand sap-water
           :demand-source (if space-svm-3 "3d-lm" "2d-lm")})))

     (when svm-result
       {:annual-demand (/ (aget svm-result 0) 0.65)
        :sap-water-demand sap-water
        :demand-source (if space-svm-3 "3d-svm" "2d-svm")}))))

(def peak-constant 21.84)
(def peak-gradient 0.0004963)

(defn run-peak-model [annual-demand]
  (+ peak-constant (* annual-demand peak-gradient)))

(defn- topo [{roads :roads buildings :buildings :as state}]
  (let [crs (::geoio/crs roads)

        roads
        (topo/node-paths (::geoio/features roads))
        
        [buildings roads]
        (topo/add-connections crs (::geoio/features buildings) roads
                              :connect-to-connectors false)
        ]
    (-> state
        (assoc-in [:roads     ::geoio/features] roads)
        (assoc-in [:buildings ::geoio/features] buildings))))

(defn dedup [state]
  (-> state
      (update-in [:buildings ::geoio/features] distinct-by ::geoio/id)
      (update-in [:roads ::geoio/features] distinct-by ::geoio/id)))

(defn- table->maps
  "Given a seq of seqs whose first element is a header,
  return a seq of maps from header values to row values"
  {:test #(test/is (= (table->maps [["this" "that"]
                                    [1 2]
                                    [3 4]])
                      (list {"this" 1 "that" 2}
                            {"this" 3 "that" 4})))}
  [[header & rows]]
  (map zipmap (repeat header) rows))

(defn- read-csv-file [file & {:keys [separator] :or {separator \,}}]
  (with-open [r (io/reader file)]
    (doall (table->maps (csv/read-csv r :separator separator)))))

(defn- compose-mapping
  "Composes a series of field mappings into a function which does them.
  A mapping is a literally a map which gives a renaming of a field.
  Non nil values of the key will be copied to the value"
  {:test
   #(let [c (compose-mapping {"widget" :sprocket
                              "blink" :blonk})]
      (test/is (= (get (c {"widget" 13 "blink" nil})
                       :sprocket) 13)))}
  [fields]
  (apply comp
         (for [[field-name field-purpose] fields]
           (fn [x]
             (let [val (get x field-name)]
               (if (and val
                        (or (not (string? val))
                            (not (string/blank? val))))
                 (assoc x field-purpose val)
                 x))))))

(defn- compose-joins
  "Turns a list of joins and some tables into a function which runs the joins.

  Each join is given by a map keyed :table-file :table-column :gis-file :gis-column.
  The :table- parts are on the RHS and the :gis- parts are on the left.

  The tables should be a sequence of maps. Those which have table-column = gis-column will
  get merged onto the input.

  TODO FIXME we can end up replacing the keys in the LHS with keys from the RHS, which could break subsequent joins"
  {:test
   #(let [c (compose-joins
             [{:table-file "table"
               :table-column "a"
               :gis-column "b"}]
             {"table" [{"a" 1 :hello :world}
                       {"a" 2 :hello :i-love-you}]})
          ]

      (test/is (= :world (get (c {"b" 1}) :hello)))
      (test/is (= :i-love-you (get (c {"b" 2}) :hello))))}
  
  [joins tables]

  (apply comp
         (for [{table :table-file
                table-column :table-column
                gis-column :gis-column}
               joins
               :let [table (get tables table)]
               :when table
               :let [table (assoc-by table #(get % table-column))]]
           (fn [x]
             (merge x (get table (get x gis-column)))))))

(defn primary-file
  "Since some GIS formats have several parts, we have to put them in a directory.
  This function turns a file or directory into the important file therein"
  [file-or-directory]
  
  (let [file-or-directory (io/file file-or-directory)]
    (cond
      (.isDirectory file-or-directory)
      (let [contents (.listFiles file-or-directory)
            n (count contents)]
        (cond
          (zero? n) nil
          (= 1 n) (first contents)
          :else (first
                 (filter
                  #(has-extension % "shp")
                  contents))))
      (.isFile file-or-directory) file-or-directory)))

(defn- load-and-join [{files :files
                       joins :joins
                       mappings :mapping}]
  (let [files (for [[base {id :id}] files :when id]
                [base (primary-file (io/file (config :import-directory) id))])

        {gis-files true
         other-files false}
        (group-by (comp boolean
                        geoio/can-read?
                        second)
                  files)

        mappings (into {} (for [[file fields] mappings] [file (compose-mapping fields)]))
        
        other-files (for [[base file] other-files]
                      [base
                       (let [rows (case (file-extension file)
                                    "csv" (read-csv-file file)
                                    ("tab" "tsv") (read-csv-file file :separator \tab)
                                    [])
                             mapping (get mappings base identity)]
                         (map mapping rows))])
        
        other-files (into {} other-files)

        joins (group-by :gis-file joins)

        joins (for [[gis-file joins] joins]
                [gis-file (compose-joins joins other-files)])

        joins (into {} joins)
        
        all-features
        (mapcat
         (fn [[base file]]
           (let [features (::geoio/features
                           (geoio/read-from file :force-crs "EPSG:4326" :key-transform identity))
                 mapping (get mappings base identity)
                 join    (get joins base identity)]
             (map (comp join mapping) features)))
         gis-files)]
    {::geoio/crs "EPSG:4326" ::geoio/features all-features}))

(defn- produce-peak
  "Make sure the feature has a :peak-demand"
  [feature]
  (let [given-peak (as-double (:peak-demand feature))
        given-pbr  (as-double (:peak-base-ratio feature))]
    (cond
      given-peak
      (assoc feature
             :peak-demand given-peak
             :peak-source :given)
      
      given-pbr
      (assoc feature
             :peak-demand
             (let [demand-kw (annual-kwh->kw (:annual-demand feature))]
               (* (max given-pbr 1.0) demand-kw))
             :peak-source :ratio)
      
      :else
      (assoc feature
             :peak-demand (run-peak-model (:annual-demand feature))
             :peak-source :regression))))

(defn produce-demand
  "Make sure the feature has an :annual-demand"
  [feature sqrt-degree-days]
  (let [given-demand (as-double (:annual-demand feature))
        given-height (as-double (:height feature))
        given-fallback-height (as-double (:fallback-height feature))
        given-floor-area (as-double (:floor-area feature))
        benchmark-m  (as-double (:benchmark-m feature))
        benchmark-c  (or (as-double (:benchmark-c feature))
                         (when benchmark-m 0))

        storey-height lidar/*storey-height*
        height     (or given-height (::lidar/height feature) given-fallback-height)

        floor-area (or given-floor-area
                       (* (Math/ceil
                           (/ (or height storey-height)
                              storey-height))
                          (::lidar/footprint feature)))

        residential (if (contains? feature :residential)
                      ;; the double boolean here means that if
                      ;; as-boolean says nil (which means 'doesn't look like a boolean')
                      ;; we just go false instead.
                      (boolean (as-boolean (:residential feature)))
                      (contains? residential-subtypes (:subtype feature)))

        feature    (assoc feature
                          ::lidar/height height
                          :height height
                          :residential residential)
        
        use-annual-demand (or (#{:use :estimate :max} (:use-annual-demand feature)) :use)

        model-output (delay (run-svm-models feature sqrt-degree-days))
        
        ;; produce demand
        feature (cond
                  (and given-demand
                       (not= :estimate use-annual-demand)
                       (or (= :use use-annual-demand)
                           (and
                            (= :max use-annual-demand)
                            (>= given-demand (:annual-demand @model-output)))))

                  ;; we only use the given demand if it exceeds the model output
                  (assoc feature
                         :annual-demand given-demand
                         :demand-source use-annual-demand)

                  benchmark-c
                  (assoc feature
                         :annual-demand (+ benchmark-c
                                           (* (or benchmark-m 0) floor-area))
                         :demand-source :benchmark)

                  :else
                  (merge feature @model-output))
        ]
    
    (assoc feature
           :sap-water-demand (sap/hot-water floor-area)
           )))

(defn- should-explode?
  "If a feature is going to end up with a summable prediction of demand,
  it should be exploded. Otherwise we can leave it alone."
  [feature]

  (and
   (#{:multi-polygon :multi-point} (::geoio/type feature))
   (not (or (:annual-demand feature)
            (and (:given-floor-area feature)
                 (or (:benchmark-m feature)
                     (:benchmark-c feature)))))))

(defn- explode-multi-polygons
  "Some of the features in the input may be multi-polygons.
  If they are, we want to break them into parts, so we can recombine
  them later."
  [features]
  (let [count-before (count features)
        features (->> features
                      (map-indexed (fn [i x] (assoc x ::id i)))
                      (mapcat (fn [feature]
                                (let [feature
                                      (-> feature
                                          (update :annual-demand as-double)
                                          (update :given-floor-area as-double)
                                          (update :benchmark-m as-double)
                                          (update :benchmark-c as-double))]
                                  (if (should-explode? feature)
                                    (geoio/explode-multi feature)
                                    [feature])))))
        count-after (count features)
        ]
    (log/info "Exploded multipolygons from" count-before "to" count-after)
    features))

(defn- merge-multi-polygon
  "Used by `merge-multi-polygons` to combine a bunch of features that
  have the same ::id, and so came from the same input feature."
  [polygons]
  (if (= 1 (count polygons))
    (first polygons)

    (let [basis (dissoc (first polygons) ::geoio/geometry)
          geoms (map ::geoio/geometry polygons)]
      (cond-> (geoio/update-geometry
               basis
               (jts/create-multipolygon geoms))

        (not= :given (:demand-source basis))
        (assoc :annual-demand (reduce + (map :annual-demand polygons)))

        ;; if the peak-source is given, we shouldn't sum them,
        ;; but also if the demand-source is given, since for a given
        ;; demand the first one has the right peak also.
        (not (or (= :given (:peak-source basis))
                 (= :given (:demand-source basis))))
        (assoc :peak-demand (reduce + (map :peak-demand polygons)))))))

(defn merge-multi-polygons
  "The inverse of `explode-multi-polygons`."
  [features]
  (->> features
       (group-by ::id)
       (map second)
       (map merge-multi-polygon)))

(defn add-areas [building]
  (let [height (or (:height building)
                   (::lidar/height building)
                   lidar/*storey-height*)]
    (assoc building
           :wall-area   (::lidar/external-wall-area
                         building
                         (* (- (::lidar/perimeter building 0)
                               (::lidar/shared-perimeter building 0))
                            height))
           :floor-area  (::lidar/floor-area building 0)
           :ground-area (::lidar/footprint building 0)
           :roof-area   (::lidar/footprint building 0)
           :height      height)))

(defn run-import
  "Run an import job enqueued by `queue-import`"
  [map-id map-name parameters progress]
  (let [work-directory (util/create-temp-directory!
                        (config :import-directory)
                        (str
                         "map-" map-id "-"
                         (str/replace (or map-name "????") #"[^a-zA-Z0-9]+" "-") "-"))

        osm-buildings (-> parameters :buildings :source (= :osm))
        osm-roads     (-> parameters :roads :source (= :osm))

        progress* (fn [x p m] (progress :message m :percent p :can-cancel true) x)

        sqrt-degree-days (Math/sqrt (:degree-days parameters))
        ]
    
    (log/info "About to import" map-id map-name "in" (.getName work-directory))
    (with-open [log-writer (io/writer (io/file work-directory "log.txt"))]
      (binding [*out* log-writer]
        (pprint parameters)
        
        (try
          (-> (cond-> {:work-directory work-directory}
                ;; 1. Load any GIS files
                (not osm-buildings)
                (assoc :buildings (load-and-join (:buildings parameters)))

                (not osm-roads)
                (assoc :roads (load-and-join (:roads parameters)))
                
                ;; 2. If we need some OSM stuff, load that
                (or osm-buildings osm-roads)
                (-> (progress* 10 "Quering OpenStreetMap")
                    (query-osm parameters)))

              ;; at this point, if we have multipolygons we should
              ;; explode them so that the LIDAR processing calculates
              ;; its stuff on a per-shape basis.

              ;; step 1: subdivide multi-features into bits
              (update-in [:buildings ::geoio/features] explode-multi-polygons)
              ;; now we have several of everything, potentially
              (progress* 20 "Checking for LIDAR coverage")
              (update :buildings lidar/add-lidar-to-shapes (load-lidar-index))
              
              (progress* 30 "Computing annual demands")
              (update :buildings geoio/update-features :produce-demands
                      produce-demand sqrt-degree-days)

              ;; at this point we need to recombine anything that has
              ;; been exploded.
              (update-in [:buildings ::geoio/features] merge-multi-polygons)

              ;; we want to do peak modelling afterwards
              (progress* 35 "Computing peak demands")
              (update :buildings geoio/update-features :produce-peaks produce-peak)

              (progress* 45 "De-duplicating geometry")
              (dedup)
              (progress* 50 "Noding paths and adding connectors")
              (topo)
              (progress* 80 "Adding map to database")

              (update :buildings geoio/update-features :add-areas add-areas))
          
          (catch Exception e
            (log/error e "Error during import: ")
            (throw e) ;; so the job gets marked failed
            ))))))

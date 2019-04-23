(ns thermos-backend.importer.core
  (:require [thermos-backend.queue :as queue]
            [thermos-backend.util :as util]
            [thermos-backend.config :refer [config]]

            [clojure.java.io :as io]
            [org.tobereplaced.nio.file :as nio]

            [thermos-importer.geoio :as geoio]
            [thermos-importer.overpass :as overpass]
            [thermos-importer.lidar :as lidar]
            [thermos-importer.svm-predict :as svm]
            [thermos-importer.spatial :as topo]
            [thermos-importer.util :refer [has-extension file-extension]]

            [thermos-backend.db.maps :as db]
            
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.data.csv :as csv]
            [thermos-importer.spatial :as spatial]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [thermos-util :refer [as-double as-boolean assoc-by distinct-by annual-kwh->kw]]
            [clojure.pprint :refer [pprint]])
  (:import [org.locationtech.jts.geom
            Envelope
            GeometryFactory
            PrecisionModel]))

(defn- primary-file
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

(defn- keyword-upcase-uscore [s]
  (keyword (str/replace s "_" "-")))

(defn- load-model [name]
  (-> name
      (io/resource)
      (slurp)
      (json/read-str :key-fn keyword-upcase-uscore)
      (svm/predictor)))

(def svm-space-3d (load-model "thermos_backend/importer/space-3d.json"))
(def svm-space-2d (load-model "thermos_backend/importer/space-2d.json"))
(def svm-water-3d (load-model "thermos_backend/importer/water-3d.json"))
(def svm-water-2d (load-model "thermos_backend/importer/water-2d.json"))

(defn queue-import
  "Put an import job into the queue - at the moment :map-id is the only
  argument, as the map itself contains the import details."
  [{map-id :map-id}]
  (queue/enqueue :imports {:map-id map-id}))

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

        _ (println "Query area:" query-area)
        
        set-building-type  #(assoc % :subtype (remap-building-type %))
        set-road-type      #(assoc % :subtype (clean-tag (:highway %)))
        
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
             (map set-building-type))

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

(defn- run-svm-models [x degree-days]
  (let [x (->> (for [[k v] x
                     :when (and (keyword? k)
                                (or (= :residential k)
                                    (and v (= "thermos-importer.lidar" (namespace k)))))]
                 [(keyword (name k))
                  (if (= :residential k) (boolean v) v)])
               (into {}))
        space3 (svm-space-3d x)
        water3 (svm-water-3d x)
        space-prediction  (or space3 (svm-space-2d x) 0)
        water-prediction  (or water3 (svm-water-2d x) 0)
        space-requirement (* space-prediction (Math/sqrt degree-days))]
    {:annual-demand (+ space-prediction water-prediction)
     :demand-source (if space3 :svm-3 :svm-2)}))

(def peak-constant 21.84)
(def peak-gradient 0.0004963)

(defn- run-peak-model [annual-demand]
  (+ peak-constant (* annual-demand peak-gradient)))

(defn- topo [{roads :roads buildings :buildings :as state}]
  (let [crs (::geoio/crs roads)

        roads
        (topo/node-paths (::geoio/features roads))
        
        [buildings roads]
        (topo/add-connections crs (::geoio/features buildings) roads)
        ]
    (-> state
        (assoc-in [:roads     ::geoio/features] roads)
        (assoc-in [:buildings ::geoio/features] buildings))))

(defn add-to-database [state job]
  (geoio/write-to
   (:buildings state)
   (io/file (:work-directory job) "buildings-out.json"))

  (println "Demand models used:"
           (frequencies (map :demand-source
                             (::geoio/features (:buildings state)))))
  
  (geoio/write-to
   (:roads state)
   (io/file (:work-directory job) "roads-out.json"))

  ;; these should already be in 4326???
  (let [buildings (geoio/reproject (:buildings state) "EPSG:4326")
        roads     (geoio/reproject (:roads state)     "EPSG:4326")
        box       (geoio/bounding-box
                   buildings
                   (geoio/bounding-box roads))
        
        gf        (GeometryFactory. (PrecisionModel.) 4326)
        ]

    (db/insert-into-map!
     :map-id (:map-id job)
     
     :erase-geometry (.toText (.toGeometry gf box))
     :srid 4326
     :format :wkt
     :buildings

     ;; uppercase the hyphens for database :(
     (for [b (::geoio/features buildings)]
       {:id (::geoio/id b)
        :orig-id (or (:identity b) "unknown")
        :name (or (:name b) "")
        :type (or (:subtype  b) "")
        :geometry (.toText (::geoio/geometry b))
        ;; insert array type here??
        :connection-id (str/join "," (::spatial/connects-to-node b))
        :demand-kwh-per-year (or (:annual-demand b) 0)
        :demand-kwp (or (:peak-demand b) 0)
        :connection-cost (or (:connection-cost b) 0)
        :connection-count (or (:connection-count b) 1)})
     
     :paths
     (for [b (::geoio/features roads)]
       {:id (::geoio/id b)
        :orig-id (or (:identity b) "unknown")
        :name (or (:name b) "")
        :type (or (:subtype b) "")
        :geometry (.toText (::geoio/geometry b))
        :start-id (::geoio/id (::topo/start-node b))
        :end-id   (::geoio/id (::topo/end-node b))
        :length   (or (::topo/length b) 0)
        :fixed-cost (or (:fixed-cost b) 0)
        :variable-cost (or (:variable-cost b) 0)}))))

(defn dedup [state]
  (-> state
      (update-in [:buildings ::geoio/features] distinct-by ::geoio/id)
      (update-in [:roads ::geoio/features] distinct-by ::geoio/id)))

(defn- table->maps
  "Given a seq of seqs whose first element is a header,
  return a seq of maps from header values to row values"
  {:test #(assert (= (table->maps [["this" "that"]
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
      (assert (= (get (c {"widget" 13 "blink" nil})
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

      (assert (= :world (get (c {"b" 1}) :hello)))
      (assert (= :i-love-you (get (c {"b" 2}) :hello))))}
  
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

(defn- produce-demand
  "Make sure the feature has an :annual-demand and a :peak-demand"
  [feature degree-days]
  (let [given-demand (as-double (:annual-demand feature))
        given-height (as-double (:height feature))
        given-floor-area (as-double (:floor-area feature))
        benchmark-m  (as-double (:benchmark-m feature))
        benchmark-c  (or (as-double (:benchmark-c feature))
                         (when benchmark-m 0))
        

        storey-height lidar/*storey-height*
        height     (or given-height (::lidar/height feature))
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

        ;; produce demand
        feature (cond
                  (and given-demand (not (zero? given-demand))) ;; is this sensible? I have no idea
                  (assoc feature
                         :annual-demand given-demand
                         :demand-source :given)

                  benchmark-c
                  (assoc feature
                         :annual-demand (+ benchmark-c (if (not (zero? (or benchmark-m 0)))
                                                         (* benchmark-m floor-area)))
                         :demand-source :benchmark)

                  :else
                  (merge feature
                         (run-svm-models (assoc feature
                                                :residential residential
                                                ::lidar/height height)
                                         degree-days)))

        ;; produce peak
        given-peak (as-double (:peak-demand feature))
        given-pbr  (as-double (:peak-base-ratio feature))

        feature (cond
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
                         :peak-source :regression))]
    feature))

(defn- add-defaults [job {connection-cost :default-connection-cost
                          fixed-civil :default-fixed-civil-cost
                          variable-civil :default-variable-civil-cost}]
  (-> job
      (update :buildings geoio/update-features :add-defaults
              #(-> % (update :connection-cost (fn [x] (or x connection-cost)))))
      (update :roads geoio/update-features :add-defaults
              #(-> %
                   (update :fixed-cost (fn [x] (or x fixed-civil)))
                   (update :variable-cost (fn [x] (or x variable-civil)))
                   ))))

(defn run-import
  "Run an import job enqueued by `queue-import`"
  [{map-id :map-id} progress]
  (let [{map-name :name parameters :parameters} (db/get-map map-id)
        work-directory (util/create-temp-directory!
                        (config :import-directory)
                        (str (str/replace map-name #"[^a-zA-Z0-9]+" "-")
                             "-"))

        osm-buildings (-> parameters :buildings :source (= :osm))
        osm-roads     (-> parameters :roads :source (= :osm))

        progress* (fn [x p m] (progress :message m :percent p :can-cancel true) x)
        ]
    
    (log/info "About to import" map-id map-name "in" (.getName work-directory))
    (with-open [log-writer (io/writer (io/file work-directory "log.txt"))]
      (binding [*out* log-writer]
        (pprint parameters)
        
        (try
          (-> (cond-> {}
                ;; 1. Load any GIS files
                (not osm-buildings)
                (assoc :buildings (load-and-join (:buildings parameters)))

                (not osm-roads)
                (assoc :roads (load-and-join (:roads parameters)))
                
                ;; 2. If we need some OSM stuff, load that
                (or osm-buildings osm-roads)
                (-> (progress* 10 "Query OSM")
                    (query-osm parameters)))

              (progress* 20 "LIDAR")
              (update :buildings lidar/add-lidar-to-shapes (load-lidar-index))
              (progress* 30 "Run demand model")
              (update :buildings geoio/update-features :produce-demands
                      produce-demand (:degree-days parameters))

              (progress* 40 "Add defaults")
              (add-defaults parameters)
              (progress* 45 "Deduplicate")
              (dedup)
              (progress* 50 "Node")
              (topo)
              (progress* 80 "Database insert") ;; this is the last
                                               ;; place it's safe to
                                               ;; cancel
              (add-to-database (assoc parameters
                                      :work-directory work-directory
                                      :map-id map-id)))
          (catch Exception e
            (println "Error during import: " (.getMessage e))
            (.printStackTrace e)))))))

(queue/consume :imports 1 run-import)

(defn get-file-info [file-or-directory]
  (let [file (-> file-or-directory io/file primary-file)]
    (when file
      ;; try and open the file and get info on it!!!
      (let [extension (file-extension file)]
        (cond
          (geoio/can-read? file)
          ;; get geospatial info
          (let [{features ::geoio/features} (geoio/read-from file :key-transform identity) 
                all-keys (into #{} (mapcat keys features))
                geometry-types (into #{} (map ::geoio/type features))]
            {:keys (set (filter string? all-keys))
             :geometry-types geometry-types})
          
          (= "csv" extension)
          {:keys (set
                  (with-open [r (io/reader file)]
                    (first (csv/read-csv r))))}
          
          (#{"tsv" "tab"} extension)
          {:keys (set
                  (with-open [r (io/reader file)]
                    (first (csv/read-csv r :separator \tab))))})))))


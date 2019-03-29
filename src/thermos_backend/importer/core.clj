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
            [clojure.string :as string])
  (:import [org.locationtech.jts.geom
            Envelope
            GeometryFactory
            PrecisionModel]))


(defn- check-interrupt
  ([x] (check-interrupt) x)
  ([] (when (.isInterrupted (Thread/currentThread))
        (throw (InterruptedException.)))))

(defn- as-double [v]
  (if (string? v)
    (try (Double/parseDouble v)
         (catch NumberFormatException e))
    v))

(defn- get-double [x k]
  (and k (when-let [v (get x k)]
           (as-double v))))

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
  "
  Enqueue an import job into the import jobs queue.

  The steps here are:
  - Put the file inputs into storage directory
  - Stick it into a queue
  "
  [args]
  (let [temp-dir (util/create-temp-directory!
                  (config :import-directory)
                  (or (str/replace (:osm-area args) #"[^a-zA-Z0-9]+" "-") "file-"))
        buildings (io/file temp-dir "buildings")
        roads (io/file temp-dir "roads")
        benchmarks (io/file temp-dir "benchmarks")]

    (let [move-files-into
          (fn [files dir]
            ;; the input files are actually from
            ;; ring.middle.ware.multipart-params, which means they are
            ;; maps which have keys :filename :tempfile.
            ;; sadly, shapefiles have parts which are related only by name
            ;; so if a user has uploaded several shapefiles with the same name
            ;; from separate directories they will clobber each other.
            
            (when (seq files)
              (.mkdir dir)
              (doall
               (for [{filename :filename
                      file :tempfile} files]
                 ;; TODO ensure filename is safe
                 (let [target (io/file dir (or filename (.getname file)))]
                   (.toString (nio/move! file target)))))))

          args (-> args
                   (assoc :work-directory   (str temp-dir))
                   (update :buildings-file  move-files-into buildings)
                   (update :roads-file      move-files-into roads)
                   (update :benchmarks-file move-files-into benchmarks))
          ]
      (queue/enqueue :imports args))))

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

(defn- query-osm [state job]
  (let [get-roads          (not (:roads-file job))
        get-buildings      (not (:buildings-file job))
        set-building-type  #(assoc % :subtype (remap-building-type %))
        set-road-type      #(assoc % :subtype (clean-tag (:highway %)))]
    (cond-> state
      (or get-roads get-buildings)
      (as-> state
          (let [query-results
                (overpass/get-geometry
                 (:osm-area job)
                 :include-buildings get-buildings
                 :include-highways get-roads)

                query-results
                (for [r query-results]
                  (assoc r
                         :orig-id (:osm-id r)
                         :source (str "osm:" (:osm-area job))))
                
                buildings
                (->> query-results
                     (filter :building)
                     (filter (comp #{:polygon} ::geoio/type))
                     (map set-building-type))

                highways
                (->> query-results
                     (filter :highway)
                     (filter (comp #{:line-string} ::geoio/type))
                     (map set-road-type))
                ]
            
            (cond-> state
              get-buildings
              (assoc :buildings
                     {::geoio/crs "EPSG:4326" ::geoio/features buildings})

              get-roads
              (assoc :roads
                     {::geoio/crs "EPSG:4326" ::geoio/features highways})))))))

(defn- distinct-by [v f]
  (let [seen (volatile! #{})]
    (reduce
     (fn [a v]
       (let [vf (f v)]
         (if (@seen vf)
           (do
             (println "Removing duplicate:" vf v)
             a)
           (do (vswap! seen conj vf)
               (cons v a)))))
     nil v)))

(defn- assoc-when [m k v] (if v (assoc m k v) m))
(defn- get-double [m k]
  (when-let [v (get m k)]
    (cond
      (number? v) v
      (string? v) (try (Double/parseDouble v) (catch NumberFormatException e))
      :else nil)))

;; this way the last non-nil one wins for each feature.
(defn- copy-fields [feature field-map]
  (reduce
   (fn [feature {field-name :field-name field-target :field-target}]
     (case field-target
       :height           (assoc-when feature ::lidar/height (get-double feature field-name))
       :floor-area       (assoc-when feature ::lidar/floor-area (get-double feature field-name))
       :unit-cost        (assoc-when feature :unit-cost (get-double feature field-name))
       :connection-count (assoc-when feature :connection-count (get-double feature field-name))
       :demand           (assoc-when feature :demand-kwh-per-year (get-double feature field-name))
       :peak             (assoc-when feature :demand-kwp (get-double feature field-name))
       :classification   (assoc-when feature :subtype (get feature field-name))
       :orig-id          (assoc-when feature :orig-id (get feature field-name))
       :name             (assoc-when feature :name (get feature field-name))

       feature)) ;; do nothing
   feature field-map))

(defn- load-buildings [state
                       {buildings-file :buildings-file
                        buildings-fields :buildings-field-map}]
  (cond-> state
    (seq buildings-file)
    (assoc :buildings
           (cond->
               (geoio/read-from-multiple buildings-file :force-crs "EPSG:4326")

             (seq buildings-fields) ;; only if there is something to do
             (geoio/update-features :copy-fields copy-fields
                                    (for [f buildings-fields]
                                      (update f :field-name keyword)))))))

(defn- load-roads [state
                   {roads-file :roads-file
                    roads-fields :roads-field-map}]
  (cond-> state
    (seq roads-file)
    (assoc :roads (cond->
                      (geoio/read-from-multiple roads-file :force-crs "EPSG:4326")

                    (seq roads-fields)
                    (geoio/update-features :copy-fields copy-fields
                                           (for [f roads-fields]
                                             (update f :field-name keyword)))
                    ))))



(defn- load-lidar-index []
  (when-let [lidar-directory (config :lidar-directory)]
    (->> (file-seq (io/file ))
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

(defn- guess-residential [feature]
  (if (contains? feature :residential) feature
      ;; otherwise make it up
      (assoc feature :residential
             (contains? residential-subtypes
                        (:subtype feature)))))

(defn- produce-predictors [state
                           {use-lidar    :use-lidar}]
  (-> state
      (update :buildings
              lidar/add-lidar-to-shapes ;; TODO remove given-height, accept input height
              (when use-lidar (load-lidar-index)))

      (update :buildings
              geoio/update-features :add-resi
              guess-residential)))

(defn- run-svm-models [x degree-days]
  (let [x (->> (for [[k v] x
                     :when (or (= :residential k)
                               (and v (= "thermos-importer.lidar" (namespace k))))]
                 [(keyword (name k))
                  (if (= :residential k) (boolean v) v)])
               (into {}))
        space-prediction  (or (svm-space-3d x) (svm-space-2d x) 0)
        water-prediction  (or (svm-water-3d x) (svm-water-2d x) 0)
        space-requirement (* space-prediction (Math/sqrt degree-days))]
    (+ space-prediction water-prediction)))

(def peak-constant 21.84)
(def peak-gradient 0.0004963)

(defn- run-peak-model [annual-demand]
  (+ peak-constant (* annual-demand peak-gradient)))

(defn- load-benchmarks
  {:test
   #(let [gadget (load-benchmarks ["cat1" "cat2" "demand-m" "demand-c" "peak-m" "peak-c"]
                                  [["a" "b" "1" "2" "3" "4"]
                                   ["c" "d" "1" "2" "3" "4"]])
          result (gadget {:cat1 "a" :cat2 "b" :thermos-importer.lidar/floor-area 2})
          ]
      (assert
       (= result
          {:demand-kwh-per-year 4.0 :demand-kwp 10.0})))
   }
  [header rows]

  (let [header (map keyword header)
        rows   (map zipmap (repeat header) rows)
        rows   (for [row rows]
                 (-> row
                     (update :demand-m as-double)
                     (update :demand-c as-double)
                     (update :peak-m as-double)
                     (update :peak-c as-double)))
        
        header (set header)
        predictors (disj header :demand-m :demand-c :peak-m :peak-c)
        predictors (vec predictors)
        get-predictors #(vec (doall (for [p predictors]
                                      (str/trim (str/lower-case (str (get % p)))))))
        
        rows-by-predictors (group-by get-predictors rows)
        ]
    (fn [x]
      (let [predictors (get-predictors x)
            rows (rows-by-predictors predictors)
            row (first rows)]
        (when row
          (let [{demand-m :demand-m demand-c :demand-c
                 peak-m :peak-m     peak-c :peak-c}
                row]
            ;; TODO benchmarks require floor area which
            ;; requires storey height, but I just took
            ;; that out of the lidar doodad ↓
            (let [floor-area (::lidar/floor-area x)

                  demand
                  (when (and floor-area demand-m demand-c)
                    (+ demand-c (* demand-m floor-area)))
                  
                  peak
                  (when (and floor-area peak-m peak-c)
                    (+ peak-c (* peak-m floor-area)))
                  ]
              {:demand-kwh-per-year demand :demand-kwp peak})))))))

(def default-benchmark-predictor
  (let [[header & rows]
        (with-open [rdr (io/reader
                         (io/resource "thermos_backend/importer/default-benchmarks.csv"))]
          (doall (csv/read-csv rdr)))]
    (load-benchmarks header rows)))

(defn- produce-demands [state
                        {benchmarks-file    :benchmarks-file
                         default-benchmarks :default-benchmarks
                         degree-days        :degree-days}]
  (let [benchmark-predictors
        (cond-> (map #(let [[header & rows]
                            (with-open [rdr (io/reader %)]
                              (doall (csv/read-csv rdr)))]
                        (load-benchmarks header rows))
                     benchmarks-file)
          default-benchmarks
          (conj default-benchmark-predictor))

        get-benchmark
        (if (seq benchmark-predictors)
          (fn [x]
            (reduce merge {} (map #(% x) benchmark-predictors)))
          (constantly {}))

        estimate-demand
        (fn [x]
          (let [b (get-benchmark x)

                annual-demand (or (:demand-kwh-per-year x)
                                  (:demand-kwh-per-year b)
                                  (run-svm-models x degree-days))

                peak-demand (or (:demand-kwp x)
                                (:demand-kwp b)
                                (run-peak-model annual-demand))]
            (assoc x
                   :demand-kwh-per-year annual-demand
                   :demand-kwp peak-demand)))
        ]
    (update state :buildings
            geoio/update-features
            :estimate-demand
            estimate-demand)))

(defn- topo [{roads :roads buildings :buildings :as state} job]
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

  (geoio/write-to
   (:roads state)
   (io/file (:work-directory job) "roads-out.json"))

  (let [buildings (geoio/reproject (:buildings state) "EPSG:4326")
        roads     (geoio/reproject (:roads state)     "EPSG:4326")
        box       (Envelope.)
        gf        (GeometryFactory. (PrecisionModel.) 4326)
        ]
    
    (doseq [{g ::geoio/geometry}
            (concat (::geoio/features buildings)
                    (::geoio/features roads))]
      (.expandToInclude box (.getEnvelopeInternal g)))

    (db/insert-into-map!
     :map-id (:map-id job)
     
     :erase-geometry (.toText (.toGeometry gf box))
     :srid 4326
     :format :wkt
     :buildings

     ;; uppercase the hyphens for database :(
     
     (for [b (::geoio/features buildings)]
       {:id (::geoio/id b)
        :orig-id (or (:orig-id b) "unknown")
        :name (or (:name b) "")
        :type (or (:subtype  b) "")
        :geometry (.toText (::geoio/geometry b))
        ;; insert array type here??
        :connection-id (str/join "," (::spatial/connects-to-node b))
        :demand-kwh-per-year (or (:demand-kwh-per-year b) 0)
        :demand-kwp (or (:demand-kwp b) 0)
        :connection-count (or (:connection-count b) 1)})
     
     :paths
     (for [b (::geoio/features roads)]
       {:id (::geoio/id b)
        :orig-id (or (:orig-id b) "unknown")
        :name (or (:name b) "")
        :type (or (:subtype b) "")
        :geometry (.toText (::geoio/geometry b))
        :start-id (::geoio/id (::topo/start-node b))
        :end-id   (::geoio/id (::topo/end-node b))
        :length   (or (::topo/length b) 0)
        :unit-cost (or (:unit-cost b) 500)}))))


(defn dedup [state]
  (-> state
      (update-in [:buildings ::geoio/features] distinct-by ::geoio/id)
      (update-in [:roads ::geoio/features] distinct-by ::geoio/id)))

(defn- status [x str]
  (println str) x)

(defn run-import
  "Run an import job enqueued by `queue-import`"
  [job]
  (with-open [log-writer (io/writer (io/file (:work-directory job) "log.txt"))]
    (binding [*out* log-writer ]
      (-> {}
          (status "Querying OSM...")
          (query-osm job)
          (check-interrupt)

          (status "Loading buildings...")
          (load-buildings job)
          (check-interrupt)

          (status "Creating predictors...")
          (produce-predictors job)
          (check-interrupt)

          (status "Running demand models...")
          (produce-demands job)
          (check-interrupt)

          (status "Loading roads...")
          (load-roads job)
          (check-interrupt)

          (status "Removing duplicates...")
          (dedup)
          (check-interrupt)

          (status "Noding...")
          (topo job)
          (check-interrupt)

          (status "Adding to database...")
          (add-to-database job)

          (status "Finished!")))))

(queue/consume :imports 1 run-import)

(defn- primary-file [file-or-directory]
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
    (.isFile file-or-directory) file-or-directory))

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
          

          (or (= "tsv" extension)
              (= "tab" extension))
          {:keys (set
                  (with-open [r (io/reader file)]
                    (first (csv/read-csv r :separator \t))))})))))


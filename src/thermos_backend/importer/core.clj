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

            [thermos-backend.maps.db :as map-db]
            
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.data.csv :as csv])
  (:import [org.locationtech.jts.geom Envelope]))

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
                  ;; TODO this is unsafe
                  (or (str/replace (:osm-area args) #"[^a-zA-Z0-9]+" "-") "-import"))
        buildings (io/file temp-dir "buildings")
        roads (io/file temp-dir "roads")
        benchmarks (io/file temp-dir "benchmarks")]

    (.mkdir buildings)
    (.mkdir roads)
    (.mkdir benchmarks)

    (let [move-files-into
          (fn [files dir]
            (when (seq files)
              (doall
               (for [file files]
                 (.toString
                  (nio/move! file (io/file dir (.getName file))))))))

          args (-> args
                   (assoc :work-directory   (str temp-dir))
                   (update :buildings-file  move-files-into buildings)
                   (update :roads-file      move-files-into roads)
                   (update :benchmarks-file move-files-into benchmarks))
          ]
      (queue/enqueue :imports args))))

(defn- query-osm [state job]
  (let [get-roads (not (:roads-file job))
        get-buildings (not (:buildings-file job))]
    (cond-> state
      (or get-roads get-buildings)
      (as-> state
          (let [query-results
                (overpass/get-geometry
                 (:osm-area job)
                 :include-buildings get-buildings :include-highways get-roads)

                ;; TODO harmonise OSM classifications here

                buildings
                (filter #(and (:building %)
                              (= :polygon (::geoio/type %)))
                        query-results)

                highways
                (filter #(and (:highway %)
                              (= :line-string (::geoio/type %)))
                        query-results)
                ]
            (cond-> state
              get-buildings
              (assoc :buildings
                     {::geoio/crs "EPSG:4326" ::geoio/features buildings})

              get-roads
              (assoc :roads
                     {::geoio/crs "EPSG:4326" ::geoio/features highways})))))))

(defn- load-buildings [state job]
  (cond-> state
    (:buildings-file job)
    (assoc :buildings (geoio/read-from-multiple (:buildings-file job)))))

(defn- load-lidar-index []
  (when-let [lidar-directory (config :lidar-directory)]
    (->> (file-seq (io/file ))
         (filter #(and (.isFile %)
                       (let [name (.getName %)]
                         (or (.endsWith name ".tif")
                             (.endsWith name ".tiff")))))
         (lidar/rasters->index))))

(defn- produce-predictors [state
                           {given-height :given-height
                            use-lidar    :use-lidar}]
  (-> state
      (update :buildings
              lidar/add-lidar-to-shapes
              (when use-lidar (load-lidar-index))
              :given-height given-height)
      (update :buildings
              geoio/update-features :add-resi
              assoc :residential true
              ;; TODO determine residential field here - maybe if it's
              ;; not false but is missing set it to true?
              )))

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
        (with-open [rdr (io/reader (io/resource "thermos_backend/importer/default-benchmarks.csv"))]
          (doall (csv/read-csv rdr)))]
    (load-benchmarks header rows)))

(defn- produce-demands [state
                        {benchmarks-file    :benchmarks-file
                         default-benchmarks :default-benchmarks
                         given-demand       :given-demand
                         given-peak         :given-peak
                         degree-days        :degree-days}]
  (let [benchmark-predictors
        (cond-> (map #(let [[header & rows]
                            (with-open [rdr (io/reader source)]
                              (doall (csv/read-csv rdr)))]
                        (load-benchmarks header rows))
                     benchmarks-file)
          default-benchmarks
          (conj default-benchmark-predictor))

        get-benchmark
        (fn [x]
          (reduce merge {} (map #(% x) benchmark-predictors)))

        estimate-demand
        (fn [x]
          (let [b (get-benchmark x)

                annual-demand (or (get-double x given-demand)
                                  (:demand-kwh-per-year b)
                                  (run-svm-models x degree-days))

                peak-demand (or (get-double x given-peak)
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


(defn- load-roads [state job]
  (cond-> state
    (:roads-file job)
    (assoc :roads (geoio/read-from-multiple (:roads-file job)))))

(defn- topo [{roads :roads buildings :buildings :as state} job]
  (let [crs (::geoio/crs roads)

        roads
        (topo/node-paths (::geoio/features roads))
        
        [buildings roads]
        (topo/add-connections crs
                              (::geoio/features buildings)
                              (::geoio/features roads))
        ]
    (-> state
        (assoc-in [:roads     ::geoio/features] roads)
        (assoc-in [:buildings ::geoio/features] buildings))))

(defn add-to-database [state]
  ;; let's also save it into the tenmp directory
  (geoio/write-to
   (:buildings state)
   (io/file (:work-directory state) "buildings-out.json"))

  (geoio/write-to
   (:roads state)
   (io/file (:work-directory state) "roads-out.json"))

  (let [buildings (geoio/reproject (:buildings state) "EPSG:4326")
        roads     (geoio/reproject (:roads state)     "EPSG:4326")
        box       (Envelope.)]
    (doseq [{g ::geoio/geometry}
            (concat (::geoio/features buildings)
                    (::geoio/features roads))]
      (.expandToInclude box (.getEnvelopeInternal g)))
    ;; TODO actually mangle the database here
    ))

(defn run-import
  "Run an import job enqueued by `queue-import`"
  [job]

  (-> {}
      (query-osm job)

      (load-buildings job)
      (produce-predictors job)
      (produce-demands job)
      
      (load-roads job)
      (topo job)
      (add-to-database)))

(queue/consume :imports run-import)

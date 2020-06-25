(ns thermos-backend.importer.core
  (:require [thermos-backend.queue :as queue]
            [thermos-backend.importer.process :refer :all]
            [thermos-backend.util :as util]
            [thermos-backend.config :refer [config]]

            [clojure.set :as set]
            [clojure.java.io :as io]
            [org.tobereplaced.nio.file :as nio]
            [mount.core :refer [defstate]]

            [thermos-importer.geoio :as geoio]
            [thermos-importer.overpass :as overpass]
            [thermos-importer.lidar :as lidar]
            [thermos-importer.svm-predict :as svm]
            [thermos-importer.lm-predict :as lm]
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
            [clojure.pprint :refer [pprint]]
            [cljts.core :as jts]
            [clojure.test :as test])
  
  (:import [org.locationtech.jts.geom
            Envelope
            GeometryFactory
            PrecisionModel]))

(defn queue-import
  "Put an import job into the queue - at the moment :map-id is the only
  argument, as the map itself contains the import details."
  [{map-id :map-id}]
  (let [job-id (queue/enqueue :imports {:map-id map-id})]
    (db/set-job-id! map-id job-id)))

(defn add-to-database [state job]
  (geoio/write-to
   (:buildings state)
   (io/file (:work-directory state) "buildings-out.json"))

  (geoio/write-to
   (:roads state)
   (io/file (:work-directory state) "roads-out.json"))

  ;; these should already be in 4326???
  (let [buildings (geoio/reproject (:buildings state) "EPSG:4326")
        roads     (geoio/reproject (:roads state)     "EPSG:4326")
        box       (geoio/bounding-box
                   buildings
                   (geoio/bounding-box roads))
        
        gf        (GeometryFactory. (PrecisionModel.) 4326)
        ]

    (let [features (concat buildings roads)
          features-by-id (group-by ::geoio/id features)]
      
      (doseq [[id features] features-by-id]
        (when (> (count features) 1)
          (log/warn (count features)
                    "duplicate features for" id))))
    
    (db/insert-into-map!
     :map-id (:map-id job)
     
     :erase-geometry (.toText (.toGeometry gf box))
     :srid 4326
     :format :wkt
     :buildings

     (for [b (::geoio/features buildings)]
       {:geoid (::geoio/id b)
        :orig-id (or (:identity b) "unknown")
        :name (str (or (:name b) ""))
        :type (str (or (:subtype  b) ""))
        :geometry (.toText (::geoio/geometry b))

        :connection-id (str/join "," (::spatial/connects-to-node b))
        :demand-kwh-per-year (or (:annual-demand b) 0)
        :cooling-kwh-per-year (or (:annual-cooling-demand b) 0)
        :cooling-kwp (or (:cooling-peak b) 0)
        
        :demand-kwp (or (:peak-demand b) 0)
        :connection-count (or (:connection-count b) 1)
        :demand-source (name (:demand-source b))
        :peak-source (name (:peak-source b))

        :wall-area   (:wall-area b)
        
        :floor-area  (:floor-area b)
        :ground-area (:ground-area b)
        :roof-area   (:roof-area b)
        :height      (:height b)
        })
     
     :paths
     (for [b (::geoio/features roads)]
       {:geoid (::geoio/id b)
        :orig-id (or (:identity b) "unknown")
        :name (str (or (:name b) ""))
        :type (str (or (:subtype b) ""))
        :geometry (.toText (::geoio/geometry b))
        :start-id (::geoio/id (::topo/start-node b))
        :end-id   (::geoio/id (::topo/end-node b))
        :length   (or (::topo/length b) 0)
        })))
  )

(defn run-import-and-store
  "Run an import job enqueued by `queue-import`"
  [{map-id :map-id} progress]
  
  (let [{map-name :name parameters :parameters} (db/get-map map-id)]
    (if (empty? parameters)
      (log/error "Map" map-id "does not exist - probably been deleted before import")
      (-> (run-import map-id map-name parameters progress)
          (add-to-database (assoc parameters :map-id map-id))))))



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

(defstate consumer
  :start
  (queue/consume :imports (config :importer-count 4) run-import-and-store))


(ns thermos-backend.lidar
  (:require [thermos-backend.config :refer [config]]
            [clojure.java.io :as io]
            [thermos-importer.lidar :as lidar]
            [cljts.core :as jts]
            [thermos-importer.geoio :as geoio]))

(defn per-project-lidar-dir 
  "directory within the system LIDAR dir that contains a separate
   directory for each projects' LIDAR tiles."
  ^java.io.File []
  (when-let [lidar-directory (config :lidar-directory)]
    (io/file lidar-directory "project")))

(defn project-lidar-dir ^java.io.File [project-id]
  (when-let [lidar-directory (config :lidar-directory)]
    (io/file lidar-directory "project" (str project-id))))

(defn project-lidar-file ^java.io.File [project-id filename]
  (io/file (project-lidar-dir project-id) filename))

(defn- reproject-coord [coord from-crs to-crs]
  (let [reprojected (geoio/reproject {::geoio/features [(geoio/geom->map coord)]
                                      ::geoio/crs from-crs} to-crs)]
    (-> reprojected
        (::geoio/features)
        (first)
        (::geoio/geometry))))

(defn- reproject-bounds [{:keys [x1 y1 x2 y2]} from-crs to-crs]
  (let [p1 (reproject-coord (jts/create-point [x1 y1]) from-crs to-crs)
        p2 (reproject-coord (jts/create-point [x2 y2]) from-crs to-crs)]
    {:x1 (.getX p1)
     :y1 (.getY p1)
     :x2 (.getX p2)
     :y2 (.getY p2)}))

(defn- is-tiff? [file]
  (and (.isFile file)
       (let [name (.getName file)]
         (or (.endsWith name ".tif")
             (.endsWith name ".tiff")))))

(defn- can-access-tiff? 
  "For a given LIDAR tiff, return true if it is not in another project's LIDAR dir."
  [project-id file]
  (let [path (.getPath file)]
    (or (not (.contains path (.getPath (per-project-lidar-dir))))
        (.contains path (.getPath (project-lidar-dir project-id))))))

(defn lidar-properties [file & {:keys [force-crs]}]
  (let [facts (lidar/raster-facts file)
        rect (:bounds facts)
        bounds {:x1 (.x1 rect)
                :y1 (.y1 rect)
                :x2 (.x2 rect)
                :y2 (.y2 rect)}
        crs (:crs facts)]
    {:filename (.getName (:raster facts))
     :crs (if force-crs force-crs crs)
     :bounds (if force-crs (reproject-bounds bounds crs force-crs) bounds)}))

(defn project-lidar-properties [project-id & {:keys [force-crs include-system-lidar]}]
  (->> (file-seq (if include-system-lidar 
                   (io/file (config :lidar-directory)) 
                   (project-lidar-dir project-id)))
       (filter is-tiff?)
       (filter #(can-access-tiff? project-id %))
       (map (fn [file] (lidar-properties file :force-crs force-crs)))))

(defn delete-project-lidar! [project-id]
  (letfn [(delete-dir [^java.io.File file]
                      (do (when (.isDirectory file)
                            (doseq [child (.listFiles file)]
                              (delete-dir child)))
                          (io/delete-file file)))]
    (delete-dir (project-lidar-dir project-id))))

(defn lidar-coverage-geojson [project-id & {:keys [include-system-lidar]}]
  (let [properties (project-lidar-properties project-id 
                                             :force-crs "EPSG:4326" 
                                             :include-system-lidar include-system-lidar)]
    {:type "FeatureCollection"
     :features
     (map (fn [{{:keys [x1 y1 x2 y2]} :bounds filename :filename}]
            {:type "Feature"
             :properties {:filename filename}
             :geometry {:type "Polygon"
                        :coordinates [[[x1 y1]
                                       [x2 y1]
                                       [x2 y2]
                                       [x1 y2]
                                       [x1 y1]]]}}) properties)}))

(defn upload-lidar! [file project-id]
  (let [files (if (vector? file) file [file])
        target-dir (project-lidar-dir project-id)]
    
    (.mkdirs target-dir)

    (doseq [file files]
      (let [target-file ^java.io.File (io/file target-dir (:filename file))]
        (java.nio.file.Files/move
         (.toPath (:tempfile file))
         (.toPath target-file)
         (into-array java.nio.file.CopyOption [java.nio.file.StandardCopyOption/REPLACE_EXISTING]))))))
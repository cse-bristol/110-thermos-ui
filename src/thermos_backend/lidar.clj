(ns thermos-backend.lidar
  (:require [thermos-backend.config :refer [config]]
            [clojure.java.io :as io]
            [thermos-importer.lidar :as lidar]
            [thermos-backend.util :as util]))

(defn project-lidar-dir ^java.io.File [project-id]
  (when-let [lidar-directory (config :lidar-directory)]
    (io/file lidar-directory "project" (str project-id))))

(defn project-lidar-file ^java.io.File [project-id filename]
  (io/file (project-lidar-dir project-id) filename))

(defn lidar-properties [file]
  (let [facts (lidar/raster-facts file)
        rect (:bounds facts)]
    {:filename (.getName (:raster facts))
     :crs (:crs facts)
     :bounds {:x1 (.x1 rect)
              :y1 (.y1 rect)
              :x2 (.x2 rect)
              :y2 (.y2 rect)}}))

(defn project-lidar-properties [project-id]
  (->> (file-seq (project-lidar-dir project-id))
       (filter #(and (.isFile %)
                     (let [name (.getName %)]
                       (or (.endsWith name ".tif")
                           (.endsWith name ".tiff")))))
       (map lidar-properties)))

(defn delete-project-lidar! [project-id]
  (letfn [(delete-dir [^java.io.File file]
                      (do (when (.isDirectory file)
                            (doseq [child (.listFiles file)]
                              (delete-dir child)))
                          (io/delete-file file)))]
    (delete-dir (project-lidar-dir project-id))))

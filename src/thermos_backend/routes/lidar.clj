(ns thermos-backend.routes.lidar
  (:require [thermos-backend.routes.responses :refer :all]
            [thermos-backend.auth :as auth]
            [ring.util.response :as response]
            [ring.util.codec :refer [url-decode]]
            [thermos-backend.db.projects :as projects]
            [thermos-backend.pages.cache-control :as cache-control]
            [thermos-backend.pages.lidar :as lidar-pages]
            [thermos-backend.config :refer [config]]
            [clojure.java.io :as io]
            [thermos-importer.lidar :as lidar]))


(defn- project-lidar-dir ^java.io.File [project-id]
  (when-let [lidar-directory (config :lidar-directory)]
    (io/file lidar-directory "project" (str project-id))))

(defn- lidar-file ^java.io.File [project-id filename]
  (io/file (project-lidar-dir project-id) filename))

(defn- raster-facts [file]
  (let [facts (lidar/raster-facts file)
        rect (:bounds facts)]
    {:filename (.getName (:raster facts))
     :crs (:crs facts)
     :bounds {:x1 (.x1 rect)
              :y1 (.y1 rect)
              :x2 (.x2 rect)
              :y2 (.y2 rect)}}))

(defn- all-facts [project-id]
  (->> (file-seq (project-lidar-dir project-id))
       (filter #(and (.isFile %)
                     (let [name (.getName %)]
                       (or (.endsWith name ".tif")
                           (.endsWith name ".tiff")))))
       (map raster-facts)))

(defn- upload-lidar! [{{:keys [file project-id]} :params}]
  (auth/verify [:modify :project project-id]
               (let [files (if (vector? file) file [file])
                     target-dir (project-lidar-dir project-id)]

                 (.mkdirs target-dir)

                 (doseq [file files]
                   (let [target-file ^java.io.File (io/file target-dir (:filename file))]
                     (java.nio.file.Files/move
                      (.toPath (:tempfile file))
                      (.toPath target-file)
                      (into-array java.nio.file.CopyOption []))))

                 (response/redirect (str "/project/" project-id "/lidar") :see-other))))

(defn- download-lidar [{{:keys [project-id filename]} :params}]
  (auth/verify [:read :project project-id]
               (-> (response/response (lidar-file project-id (url-decode filename)))
                   (response/content-type "image/tiff")
                   (response/header
                    "Content-Disposition"
                    (str "attachment; filename=\"" filename "\"")))))

(defn- list-lidar [{{:keys [project-id]} :params}]
  (auth/verify [:read :project project-id]
                 (-> (response/response (all-facts project-id))
                     (response/content-type "text/edn"))))

(defn- delete-lidar! [{{:keys [project-id filename]} :params}]
  (auth/verify [:modify :project project-id]
               (io/delete-file (lidar-file project-id (url-decode filename)))
               (response/redirect (str "/project/" project-id "/lidar") :see-other)))

(defn- lidar-info [{{:keys [project-id filename]} :params}]
  (auth/verify [:read :project project-id]
               (-> (response/response (raster-facts (lidar-file project-id (url-decode filename))))
                   (response/content-type "text/edn"))))

(defn- manage-lidar-page [{{:keys [project-id]} :params}]
  (auth/verify [:read :project project-id]
               (let [project (projects/get-project project-id)]
                 (-> (lidar-pages/manage-lidar (:name project) (all-facts project-id))
                     (html)
                     (cache-control/no-store)))))

(defn- delete-lidar-page [{{:keys [project-id filename]} :params}]
  (auth/verify [:modify :project project-id]
               (-> (lidar-pages/delete-lidar (:name (projects/get-project project-id)) filename)
                   (html)
                   (cache-control/no-store))))


(def lidar-routes 
  {""  {:get  manage-lidar-page 
        :post upload-lidar!}
   ["/info/" [#".+" :filename]] {:get lidar-info}
   ["/delete/" [#".+" :filename]] {:get delete-lidar-page
                                   :post delete-lidar!}
   ["/" [#".+" :filename]] {:get download-lidar}})
(ns thermos-backend.routes.lidar
  (:require [thermos-backend.routes.responses :refer :all]
            [thermos-backend.auth :as auth]
            [ring.util.response :as response]
            [ring.util.codec :refer [url-decode]]
            [thermos-backend.db.projects :as projects]
            [thermos-backend.pages.cache-control :as cache-control]
            [thermos-backend.pages.lidar :as lidar-pages]
            [clojure.java.io :as io]
            [thermos-backend.lidar :as lidar]))


(defn- upload-lidar-no-redirect! [{{:keys [file project-id]} :params}]
  (auth/verify [:modify :project project-id]
               (lidar/upload-lidar! file project-id)
               (response/response (lidar/lidar-coverage-geojson project-id :include-system-lidar true))))

(defn- upload-lidar! [{{:keys [file project-id]} :params}]
  (auth/verify [:modify :project project-id]
               (lidar/upload-lidar! file project-id)
               (response/redirect (str "/project/" project-id "/lidar") :see-other)))

(defn- download-lidar [{{:keys [project-id filename]} :params}]
  (auth/verify [:read :project project-id]
               (-> (response/response (lidar/project-lidar-file project-id (url-decode filename)))
                   (response/content-type "image/tiff")
                   (response/header
                    "Content-Disposition"
                    (str "attachment; filename=\"" filename "\"")))))

(defn- list-lidar [{{:keys [project-id]} :params}]
  (auth/verify [:read :project project-id]
                 (-> (response/response (lidar/project-lidar-properties project-id))
                     (response/content-type "text/edn"))))

(defn- lidar-coverage-geojson [{{:keys [project-id]} :params}]
  (auth/verify [:read :project project-id]
               (response/response (lidar/lidar-coverage-geojson project-id :include-system-lidar true))))

(defn- delete-lidar! [{{:keys [project-id filename]} :params}]
  (auth/verify [:modify :project project-id]
               (io/delete-file (lidar/project-lidar-file project-id (url-decode filename)))
               (response/redirect (str "/project/" project-id "/lidar") :see-other)))

(defn- manage-lidar-page [{{:keys [project-id]} :params}]
  (auth/verify [:modify :project project-id]
               (let [project (projects/get-project project-id)]
                 (-> (lidar-pages/manage-lidar (:name project) (lidar/project-lidar-properties project-id))
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
   "/from-wizard" {:post upload-lidar-no-redirect!}
   "/list" {:get list-lidar}
   "/coverage.json" {:get lidar-coverage-geojson}
   ["/delete/" [#".+" :filename]] {:get delete-lidar-page
                                   :post delete-lidar!}
   ["/" [#".+" :filename]] {:get download-lidar}})
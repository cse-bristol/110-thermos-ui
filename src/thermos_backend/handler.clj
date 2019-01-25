(ns thermos-backend.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.middleware :refer [wrap-canonical-redirect]]
            
            [thermos-backend.pages :as pages]
            [thermos-backend.admin-pages :as admin-pages]
            [thermos-backend.problems.routes :as problem-routes]
            [thermos-backend.maps.routes :as map-routes]
            [thermos-backend.importer.core :as importer]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults api-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.logger :as logger]
            [clojure.tools.logging :as log]
            [thermos-backend.config :refer [config]]
            [mount.core :refer [defstate]]

            [ring.util.response :as response]))

(defn remove-trailing-slash
  "Remove the trailing '/' from a URI string, if it exists."
  [^String uri]
  (if (and (not= uri "/") (.endsWith uri "/"))
    (.substring uri 0 (dec (.length uri)))
    uri))

(defn wrap-no-cache [handler]
  (if (= "true" (config :web-server-disable-cache))
    (do (log/info "Disabling caching")
        (fn [request]
          (when-let [response (handler request)]
            (assoc-in response [:headers "Cache-Control"] "no-store"))))
    handler))

(defn- file-vector [thing]
  (if (vector? thing)
    (vec (map :tempfile thing))
    [(:tempfile thing)]))

(defn- as-double [x]
  (and x (try (Double/parseDouble x)
              (catch NumberFormatException e))))

(defroutes admin-routes
  (GET "/jobs" [] (admin-pages/job-list))
  (GET "/import" [] (admin-pages/database-import))
  (POST "/import" [building-source
                   building-gis-files :<< file-vector
                   
                   road-source
                   road-gis-files :<< file-vector

                   degree-days :<< as-double
                   
                   osm-area
                   
                   use-given-demand :<< boolean
                   given-demand-field

                   use-benchmarks :<< boolean
                   benchmarks-file :<< file-vector

                   use-given-height :<< boolean
                   given-height-field

                   use-lidar :<< boolean

                   use-given-peak :<< boolean
                   given-peak-field]

        (importer/queue-import
         (cond-> {:osm-area osm-area
                  :degree-days degree-days}
           use-given-demand  (assoc :given-demand (keyword given-demand-field))
           use-benchmarks    (cond-> (seq benchmarks-file)
                               (assoc :benchmarks-file benchmarks-file)
                               :else (assoc :default-benchmarks true))
           use-given-height  (assoc :given-height (keyword given-height-field))
           use-lidar         (assoc :use-lidar true)
           use-given-peak    (assoc :given-peak (keyword given-peak-field))

           (not= building-source "openstreetmap") (assoc :buildings-file building-gis-files)
           (not= road-source "openstreetmap") (assoc :roads-file road-gis-files)))
        
        (response/redirect "jobs")))

(defroutes top-level
  (-> (context "/api" []
               problem-routes/all
               map-routes/all)
      (wrap-json-body)
      (wrap-json-response))

  (-> (routes
       (context "/admin" [] admin-routes)
       pages/all
       )
      (wrap-defaults site-defaults))
  
  (route/not-found "not-found"))

(defstate all
  :start
  (-> top-level
      (wrap-defaults api-defaults)
      (wrap-no-cache)
      (wrap-canonical-redirect remove-trailing-slash)))


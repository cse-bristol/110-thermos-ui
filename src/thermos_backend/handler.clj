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

            [ring.util.response :as response]
            [clojure.edn :as edn]))

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

(defn- file-vector
  [thing]
  (if (vector? thing) thing
      (if thing
        [thing]
        [])))

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
                   
                   use-lidar :<< boolean

                   use-benchmarks :<< boolean
                   benchmarks-file :<< file-vector
                   
                   road-field-map :<< edn/read-string
                   building-field-map :<< edn/read-string
                   ]
        (importer/queue-import
         (cond-> {:osm-area osm-area
                  :degree-days degree-days}
           use-benchmarks    (cond-> (seq benchmarks-file)
                               (assoc :benchmarks-file benchmarks-file)
                               :else (assoc :default-benchmarks true))
           use-lidar         (assoc :use-lidar true)

           (not= building-source "openstreetmap")
           (assoc :buildings-file building-gis-files
                  :buildings-field-map building-field-map)
           
           (not= road-source "openstreetmap")
           (assoc :roads-file road-gis-files
                  :roads-field-map road-field-map)))
        
        (response/redirect "jobs")))

(defroutes top-level
  (-> (context "/api" []
               problem-routes/all
               map-routes/all)
      (wrap-json-body)
      (wrap-json-response)
      )

  (-> (routes
       (context "/admin" [] admin-routes)
       pages/all)
      )
  
  (route/not-found "not-found"))

(defstate all
  :start
  (-> top-level
      (wrap-defaults (-> site-defaults
                         (assoc-in [:security :anti-forgery] false)
                         ))
      (wrap-no-cache)
      (wrap-canonical-redirect remove-trailing-slash)))


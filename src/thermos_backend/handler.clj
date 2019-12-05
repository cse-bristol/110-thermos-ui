(ns thermos-backend.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [clojure.string :as string]
            [compojure.middleware :refer [wrap-canonical-redirect remove-trailing-slash]]
            [muuntaja.middleware :as muuntaja]
            
            [ring.middleware.defaults :refer [wrap-defaults site-defaults api-defaults]]
            [ring.middleware.ssl :refer [wrap-forwarded-scheme wrap-hsts]]
            
            [ring.logger :as logger]
            [clojure.tools.logging :as log]
            [thermos-backend.config :refer [config]]
            [mount.core :refer [defstate]]

            [ring.util.response :as response]
            [ring.util.time]
            
            [clojure.edn :as edn]

            [thermos-backend.monitoring :as monitoring]
            [thermos-backend.pages.core :as pages]
            [thermos-backend.pages.cache-control :as cache-control]
            [thermos-backend.auth :as auth]
            [thermos-backend.current-uri :as current-uri]
            [clojure.java.io :as io]))

(let [resource (io/resource "etag.txt")
      etag (and resource (string/trim (slurp resource)))]
  (defn wrap-fixed-etag [handler]
    (if (string/blank? etag)
      (do (log/info "No ETag, assume development, disable cache")
          (fn [request]
            (when-let [response (handler request)]
              (cache-control/no-store response))))
      
      (do
        (log/info "Resource ETag: " etag)
        (fn [request]
          (when-let [response (handler request)]
            (cond-> response
              (not (cache-control/no-store? response))
              (-> (cache-control/etag etag)
                  (cache-control/public :max-age 3600)))
            ))))))

(defroutes monitoring-routes
  (GET "/_prometheus" []
    (-> (response/response (monitoring/formatted-metrics))
        (response/content-type "text/plain; version=0.0.4")
        (response/charset "UTF-8")
        (cache-control/no-store))))

(defroutes site-routes
  (-> auth/auth-routes (cache-control/wrap-no-store))
  monitoring-routes
  pages/page-routes)

(defstate all
  :start
  (-> site-routes
      
      ;; (wrap-stacktrace)

      (auth/wrap-auth)
      (current-uri/wrap-current-uri)
      
      (muuntaja/wrap-params)
      (muuntaja/wrap-format)
      (wrap-defaults (-> site-defaults
                         (assoc-in [:security :anti-forgery] false)))

      (wrap-forwarded-scheme)
      (wrap-hsts)
      
      (wrap-fixed-etag)))


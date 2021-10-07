;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

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

            [thermos-backend.pages.resource-etag :as etag]
            
            [clojure.java.io :as io]))

(let [resource (io/resource "git-rev.txt")
      git-rev (and resource (string/trim (slurp resource)))]
  (defn maybe-wrap-cache [handler]
    (if (string/blank? git-rev)
      (do (log/info "No ETag, assume development, disable cache")
          (fn [request]
            (when-let [response (handler request)]
              (cache-control/no-store response))))

      (let [max-age (config :web-server-max-age)]
        (fn [request]
          (when-let [response (handler request)]
            (cache-control/public response :max-age max-age)))))))


(defroutes monitoring-routes
  (GET "/thermos-metrics" []
    (-> (response/response (monitoring/formatted-metrics))
        (response/content-type "text/plain; version=0.0.4")
        (response/charset "UTF-8")
        (cache-control/no-store))))

(defroutes site-routes
  (-> auth/auth-routes (cache-control/wrap-no-store))
  monitoring-routes
  pages/page-routes)

;; (defn debug-none-match [handler]
;;   (fn [request]
;;     (let [response (handler request)]
;;       (println request)
;;       (println (dissoc response :body))
;;       response
;;       )
    
;;     )
;;   )


(defstate all
  :start
  (-> site-routes
      
      ;; (wrap-stacktrace)

      (auth/wrap-auth)
      (current-uri/wrap-current-uri)
      
      (muuntaja/wrap-params)
      (muuntaja/wrap-format)

      (etag/wrap-resources) ;; this should mean we have etag by the time we do the 304
      
      (wrap-defaults (-> site-defaults
                         (assoc-in [:security :anti-forgery] false)
                         (assoc-in [:static :resources] false)))

      (wrap-forwarded-scheme)
      (wrap-hsts)
      (maybe-wrap-cache)
      ;; (debug-none-match)

      ))



(ns thermos-backend.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
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
            [thermos-backend.current-uri :as current-uri]))

(defn wrap-cache-control [handler]
  (if (= "true" (config :web-server-disable-cache))
    (do (log/info "Disabling caching")
        (fn [request]
          (when-let [response (handler request)]
            (cache-control/no-store response))))
    (fn [request]
      (when-let [response (handler request)]
        (cache-control/public response)))))

(defn wrap-no-70s
  "When deploying with nix, the jar goes into the nix store.
  This resets its mod date to the epoch, which is bad.
  We fix that here."
  [handler]
  (let [date (ring.util.time/format-date (java.util.Date.))]
    (fn [request]
      (-> request
          (handler)
          (update-in [:headers "Last-Modified"]
                     (fn [lm]
                       (if (and (string? lm)
                                (.startsWith lm "Thu, 01 Jan 1970"))
                         date
                         lm)))))))

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
      
      (wrap-cache-control)
      (wrap-no-70s)))


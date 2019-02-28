(ns thermos-backend.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.middleware :refer [wrap-canonical-redirect]]

            [ring.middleware.defaults :refer [wrap-defaults site-defaults api-defaults]]

            [ring.logger :as logger]
            [clojure.tools.logging :as log]
            [thermos-backend.config :refer [config]]
            [mount.core :refer [defstate]]

            [ring.util.response :as response]
            [clojure.edn :as edn]

            [thermos-backend.pages.core :as pages]
            [thermos-backend.auth :as auth]
            [thermos-backend.current-uri :as current-uri]))

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

(defroutes site-routes
  auth/auth-routes
;;  data/data-routes
  pages/page-routes)

(defstate all
  :start
  (-> site-routes
      (auth/wrap-auth)
      (wrap-defaults (-> site-defaults
                         (assoc-in [:security :anti-forgery] false)))
      (wrap-no-cache)
      (current-uri/wrap-current-uri)
      (wrap-canonical-redirect remove-trailing-slash)))


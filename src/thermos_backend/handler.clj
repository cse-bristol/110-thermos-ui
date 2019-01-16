(ns thermos-backend.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.middleware :refer [wrap-canonical-redirect]]
            [thermos-backend.pages :as pages]
            [thermos-backend.problems.routes :as problem-routes]
            [thermos-backend.maps.routes :as map-routes]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.logger :as logger]
            [clojure.tools.logging :as log]
            [thermos-backend.config :refer [config]]
            [mount.core :refer [defstate]]
            ))

(defn remove-trailing-slash
  "Remove the trailing '/' from a URI string, if it exists."
  [^String uri]
  (if (and (not= uri "/") (.endsWith uri "/"))
    (.substring uri 0 (dec (.length uri)))
    uri))

(defn wrap-no-cache [handler]
  (log/info "Disabling caching")
  (fn [request]
    (when-let [response (handler request)]
      (assoc-in response [:headers "Cache-Control"] "no-store"))))

(defstate all
  :start
  (let [disable-cache
        (if (= "true" (config :web-server-disable-cache))
          wrap-no-cache identity)]
    (-> (routes
         pages/all
         (context "/api" []
                  problem-routes/all
                  map-routes/all)
         route/not-found)
        ;; middleware:
        (wrap-defaults (assoc-in site-defaults [:security :anti-forgery] false))
        (wrap-json-body)
        (wrap-json-response)
        (disable-cache)
        (wrap-canonical-redirect remove-trailing-slash)
        (logger/wrap-with-logger))))


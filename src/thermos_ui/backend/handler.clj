(ns thermos-ui.backend.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.middleware :refer [wrap-canonical-redirect]]
            [thermos-ui.backend.pages :as pages]
            [thermos-ui.backend.store-service.routes :as problem-routes]
            [thermos-ui.backend.maps.routes :as map-routes]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [thermos-ui.backend.config :refer [config]]
            [ring.logger :as logger]
            ))

(defn remove-trailing-slash
  "Remove the trailing '/' from a URI string, if it exists."
  [^String uri]
  (if (and (not= uri "/") (.endsWith uri "/"))
    (.substring uri 0 (dec (.length uri)))
    uri))

(defroutes all
  pages/all
  (context
   "/api" []
   problem-routes/all
   map-routes/map-data-routes)
  (route/not-found "<h1>404!</h1>"))

(defn wrap-no-cache [handler]
  (println "Disabling caching (dev mode)")
  (fn [request]
    (when-let [response (handler request)]
      (assoc-in response [:headers "Cache-Control"] "no-store"))))

(def app
  (-> all
      (wrap-defaults (assoc-in site-defaults [:security :anti-forgery] false))
      wrap-json-body
      wrap-json-response
      ((if (= "true" (config :disable-cache)) wrap-no-cache identity))
      (wrap-canonical-redirect remove-trailing-slash)
      (logger/wrap-with-logger)
      ))

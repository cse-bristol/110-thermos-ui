(ns thermos-ui.backend.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.middleware :refer [wrap-canonical-redirect]]
            [thermos-ui.backend.pages :as pages]
            [thermos-ui.backend.problems.routes :as problem-routes]
            [thermos-ui.backend.maps.routes :as map-routes]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.logger :as logger]
            [com.stuartsierra.component :as component]
            ))

(defn remove-trailing-slash
  "Remove the trailing '/' from a URI string, if it exists."
  [^String uri]
  (if (and (not= uri "/") (.endsWith uri "/"))
    (.substring uri 0 (dec (.length uri)))
    uri))

(defn wrap-no-cache [handler]
  (println "Disabling caching (dev mode)")
  (fn [request]
    (when-let [response (handler request)]
      (assoc-in response [:headers "Cache-Control"] "no-store"))))

(defn all [no-cache? database queue]
  "Constructing handler"
  (let [page-routes (pages/all database)
        problem-api (problem-routes/all database queue)
        map-api (map-routes/all database)
        not-found (route/not-found "<h1>Not found</h1>")
        ]

    (-> (routes
         page-routes
         (context "/api" [] problem-api map-api)
         not-found)
        

        ;; middleware:
        (wrap-defaults (assoc-in site-defaults [:security :anti-forgery] false))
        wrap-json-body
        wrap-json-response
        ((if no-cache? wrap-no-cache identity))
        (wrap-canonical-redirect remove-trailing-slash)
        (logger/wrap-with-logger))))


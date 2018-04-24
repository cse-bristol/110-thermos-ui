(ns thermos-ui.backend.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.middleware :refer [wrap-canonical-redirect]]
            [thermos-ui.backend.pages :as pages]
            [thermos-ui.backend.store-service.routes :as problem-routes]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [environ.core :refer [env]]
            ))

(defn remove-trailing-slash
  "Remove the trailing '/' from a URI string, if it exists."
  [^String uri]
  (if (and (not= uri "/") (.endsWith uri "/"))
    (.substring uri 0 (dec (.length uri)))
    uri))

(println site-defaults)

(defroutes all
  pages/all
  (context
   "/api" []
   problem-routes/all)
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
      (wrap-canonical-redirect remove-trailing-slash)
      ((if (env :disable-cache) wrap-no-cache identity))
      ))

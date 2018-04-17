(ns thermos-ui.backend.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [thermos-ui.backend.pages :as pages]
            [thermos-ui.backend.store-service.routes :as problem-routes]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [thermos-ui.backend.config :refer [config]]
            ))

(defn add-to-slash [route filename]
  (fn [{uri :uri :as request}]
    (if (.endsWith "/" uri)
      (route (update request :uri #(str % filename)))
      (route request))))

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
      ((if (= "true" (config :disable-cache)) wrap-no-cache identity))
      ))

(ns thermos-ui.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [thermos-ui.store-service.routes :as problem-routes]
            [environ.core :refer [env]]
            ))

(defn add-to-slash [route filename]
  (fn [{uri :uri :as request}]
    (if (.endsWith "/" uri)
      (route (update request :uri #(str % filename)))
      (route request))))

(defroutes all
  problem-routes/all
  (add-to-slash
   
   (route/resources "/")
   "index.html"
   ))

(defn wrap-no-cache [handler]
  (fn [request]
    (assoc-in (handler request)
              [:headers "Cache-Control"] "no-store")))

(def app
  (->
   all
   (wrap-defaults (assoc-in site-defaults [:security :anti-forgery] false))
   wrap-json-body
   wrap-json-response
   ((if (env :disable-cache) wrap-no-cache identity))))

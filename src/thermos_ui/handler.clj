(ns thermos-ui.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.json :refer [wrap-json-response]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [thermos-ui.store-service.routes :as problem-routes]
            ))

(def app
  (->
   problem-routes/all
   (wrap-defaults (assoc-in site-defaults [:security :anti-forgery] false))
   wrap-json-response))

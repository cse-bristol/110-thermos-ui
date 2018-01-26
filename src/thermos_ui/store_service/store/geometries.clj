(ns thermos-ui.store-service.store.geometries
  (:require [clojure.java.jdbc :as j]
            [environ.core :refer [env]]))

(def pg-db {:dbtype "postgresql"
            :dbname (env :pg-db-geometries)
            :host (env :pg-host)
            :user (env :pg-user)
            :password (env :pg-password)})

(defn get-buildings
  "x and y are lat/lng z is the zoom level"
  [x y z]
  (j/query pg-db
   ["select * from buildings"]))

(ns thermos-ui.backend.maps.routes
  (:require [compojure.core :refer :all]
            [thermos-ui.backend.maps.db :as db]
            [clojure.data.json :as json])
  )

(defn- as-int [s] (Integer/parseInt (re-find #"\d+" s)))

(defn- candidate->feature [candidate]
  {:type :Feature
   :geometry (json/read-str (:geometry candidate))
   :properties (dissoc candidate :geometry)})

(defroutes map-data-routes
  (GET "/map/candidates/:zoom/:x-tile/:y-tile/"
       [zoom x-tile y-tile]
       (let [candidates (db/find-tile (as-int zoom) (as-int x-tile) (as-int y-tile))]
         {:status 200
          :body {:type :FeatureCollection
                 :features (map candidate->feature candidates)}}
         ))

  (POST "/map/candidates" {{data :file} :params}
       ;; add some candidates
        (if-let [data (data :tempfile)]
          (if (db/insert! data)
            {:status 200})))

  )

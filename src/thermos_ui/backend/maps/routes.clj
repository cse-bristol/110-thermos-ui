(ns thermos-ui.backend.maps.routes
  (:require [compojure.core :refer :all]
            [thermos-ui.backend.maps.db :as db]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [ring.util.io :as ring-io]
            )
  
  )

(def MIN_ZOOM (int 14))

(defn- as-int [s] (Integer/parseInt (re-find #"\d+" s)))

(defn- candidate->feature [candidate]
  {:type :Feature
   :geometry (json/read-str (:geometry candidate))
   :properties (dissoc candidate :geometry)})

(defroutes map-data-routes
  (GET "/map/candidates/:zoom/:x-tile/:y-tile"
       [zoom x-tile y-tile]
       (if (< (as-int zoom) MIN_ZOOM)
         {:status 200
          :body {:type :FeatureCollection
                 :features []}}
         (let [candidates (db/find-tile (as-int zoom) (as-int x-tile) (as-int y-tile))]
           {:status 200
            :body {:type :FeatureCollection
                   :features (map candidate->feature candidates)}}
           ))
       )

  (POST "/map/candidates" {{data :file} :params}
        ;; add some candidates
        (if-let [data (data :tempfile)]
          {:status 200
           :body (ring-io/piped-input-stream
                  (fn [ostream]
                    (try (with-open [w (io/writer ostream)]
                           (db/insert!
                            data
                            #(do (.write w (str % "\n"))
                                 (.flush w)
                                 (.flush ostream))))
                         (catch Exception e
                           (println "Exception in insert"
                                    e)
                           )
                         )))
           }
          )))

  

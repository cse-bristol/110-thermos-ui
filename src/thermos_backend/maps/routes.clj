(ns thermos-backend.maps.routes
  (:require [compojure.core :refer :all]
            [thermos-backend.maps.db :as db]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [ring.util.io :as ring-io]
            [ring.util.response :as response]
            [clojure.tools.logging :as log]
            [thermos-backend.maps.heat-density :as heat-density])
  (:import [java.io ByteArrayInputStream]))


(def MIN_ZOOM (int 14))

(defn- as-int [s] (Integer/parseInt (re-find #"\d+" s)))

(defn- candidate->feature [candidate]
  {:type :Feature
   :geometry (:geometry candidate)
   :properties (dissoc candidate :geometry)})

(defn all [db]
  (routes
   (GET "/map/candidates/:zoom/:x-tile/:y-tile"
        [zoom :<< as-int x-tile :<< as-int y-tile :<< as-int]

        (if (< zoom MIN_ZOOM)
          {:status 200
           :body {:type :FeatureCollection
                  :features []}}
          (let [candidates (db/find-tile db zoom x-tile y-tile)]
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
                             db
                             data
                             #(do (.write w (str % "\n"))
                                  (.flush w)
                                  (.flush ostream))))
                          (catch Exception e
                            (log/error e "Attemping to insert to /map/candidates")))))
            }
           ))

   (GET "/map/density/:z/:x/:y.png"
        [z :<< as-int x :<< as-int y :<< as-int]
        ;; TODO add z-max, add db cachelayer. if storing the image
        ;; result a change to z-max invalidates the cache, plus things
        ;; will look wrong for first viewer
        (-> (let [[max-val image-bytes] (db/tile-cache db x y z)]
              (if (and max-val image-bytes)
                (heat-density/colour-float-matrix image-bytes max-val)
                (let [[max-val-here image-bytes]
                      (heat-density/density-image
                       :x x :y y :z z
                       :size 256
                       :bandwidth 30
                       :get-values
                       (partial db/density-points db))]
                  (db/tile-cache-insert db x y z max-val-here image-bytes)
                  (heat-density/colour-float-matrix image-bytes (max (or max-val 0) max-val-here)))))
            
            (ByteArrayInputStream.)
            (response/response)
            (response/content-type "image/png")))))




  

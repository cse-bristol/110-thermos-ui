(ns thermos-backend.maps.routes
  (:require [compojure.core :refer :all]
            [thermos-backend.maps.db :as db]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [ring.util.io :as ring-io]
            [ring.util.response :as response]
            [clojure.tools.logging :as log]
            [thermos-backend.maps.heat-density :as heat-density]
            [mount.core :refer [defstate]]
            )
  (:import [java.io ByteArrayInputStream]))


(def MIN_ZOOM (int 14))

(defn- as-int [s] (Integer/parseInt (re-find #"\d+" s)))

(defn- candidate->feature [candidate]
  {:type :Feature
   :geometry (:geometry candidate)
   :properties (dissoc candidate :geometry)})

(defn- generate-and-insert [x y z]
  (let [result (heat-density/density-image
                :x x :y y :z z
                :size 256
                :bandwidth 30
                :get-values db/density-points)
        [max-val-here image-bytes] result]
    (db/tile-cache-insert x y z max-val-here image-bytes)
    result))

(defstate all
  :start
  (routes
   (GET "/map/candidates/:zoom/:x-tile/:y-tile"
        [zoom :<< as-int x-tile :<< as-int y-tile :<< as-int]

        (if (< zoom MIN_ZOOM)
          {:status 200
           :body {:type :FeatureCollection
                  :features []}}
          (let [candidates (db/find-tile zoom x-tile y-tile)]
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
                            (log/error e "Attemping to insert to /map/candidates")))))
            }
           ))

   (GET "/map/raw-density/:z/:x/:y.png"
        [z :<< as-int x :<< as-int y :<< as-int]
        ;; TODO add z-max, add db cachelayer. if storing the image
        ;; result a change to z-max invalidates the cache, plus things
        ;; will look wrong for first viewer
        (-> (let [[max-val image-bytes]
                  (db/tile-cache x y z)

                  [max-here new-bytes]
                  (when-not image-bytes
                    (generate-and-insert x y z))

                  max-val (max (or max-val 0) (or max-here 0))
                  image-bytes (or image-bytes new-bytes)]
              image-bytes)
            
            (ByteArrayInputStream.)
            (response/response)
            (response/content-type "image/png")))
   
   (GET "/map/density/:z/:x/:y.png"
        [z :<< as-int x :<< as-int y :<< as-int]
        ;; TODO add z-max, add db cachelayer. if storing the image
        ;; result a change to z-max invalidates the cache, plus things
        ;; will look wrong for first viewer
        (-> (let [[max-val image-bytes]
                  (db/tile-cache x y z)

                  [max-here new-bytes]
                  (when-not image-bytes
                    (generate-and-insert x y z))

                  max-val (max (or max-val 0) (or max-here 0))
                  image-bytes (or image-bytes new-bytes)]
              (heat-density/colour-float-matrix image-bytes max-val))
            
            (ByteArrayInputStream.)
            (response/response)
            (response/content-type "image/png")))
   ))




  

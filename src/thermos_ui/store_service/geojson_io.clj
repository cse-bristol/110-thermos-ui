(ns thermos-ui.store-service.geojson-io)
  
(defn connections>geojson
  [connections]
  {:type "FeatureCollection"
   :features (map (fn [c]
                    {:type "Feature"
                     :geometry (clojure.edn/read-string (clojure.string/replace (:geometry c) ":" ""))
                     :properties (dissoc c :geometry)})
                  connections)}) 

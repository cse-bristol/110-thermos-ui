(ns thermos-ui.backend.store-service.geojson-io)
  
(defn geometry>geojson
  "Takes a map that includes a geometry geonjson stored as a string under the key :geomety and returns a geojson map."
  [geometries]
  {:type "FeatureCollection"
   :features (map (fn [c]
                    {:type "Feature"
                     :geometry (clojure.edn/read-string
                                (clojure.string/replace (:geometry c) ":" ""))
                     :properties (dissoc c :geometry)})
                  geometries)})

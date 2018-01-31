(ns thermos-ui.store-service.geojson-io)

;;https://stackoverflow.com/questions/34934948/leaflet-and-geojson#34935345

(def eg-1
  {"type" "FeatureCollection"
   "features" [{"type" "Feature"
                "geometry" {"type" "LineString"
                            "coordinates" [[-0.1243616,51.5535153]
                                           [-0.1245374,51.5536036]]}
                "properties" {"id" "e924b72772565009512917a8ef2799bc"
                              "osm_id" 206103256
                              "class" "service"
                              "node_from" "feb3a9fbe2087b3edefad3df7c4a7d48"
                              "node_to" "cae7ffe65ea2ff823ddec41b8ca4be25"
                              "name" "*** Unknown ****"
                              "length" 15.624962}}]})
(def eg-2
   {"type" "FeatureCollection"
   "features" [
               {"type" "Feature"
                "geometry" {"type" "LineString"
                            "coordinates" [[-0.091698646481862,51.5474746364529]
                                           [-0.091645265151595,51.5474308063962]]}
                "properties" {"id" "AB4D9716-082B-9F64-2943-342AE672F1B6"
                              "name" "wewe"}}]})
  
(defn connections>geojson
  [connections]
   {"type" "FeatureCollection"
   "features" [{"type" "Feature"
                "geometry" {"type" "LineString"
                            "coordinates" [[51.5535153 -0.1243616]
                                           [51.5536036 -0.1245374]]}
                "properties" {"id" "e924b72772565009512917a8ef2799bc"
                              "osm_id" 206103256
                              "class" "service"
                              "node_from" "feb3a9fbe2087b3edefad3df7c4a7d48"
                              "node_to" "cae7ffe65ea2ff823ddec41b8ca4be25"
                              "name" "*** Unknown ****"
                              "length" 15.624962}}]})
 

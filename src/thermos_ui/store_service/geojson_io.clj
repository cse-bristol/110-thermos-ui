(ns thermos-ui.store-service.geojson-io
   (:import
    [org.geotools.data FileDataStoreFinder DataUtilities]
    [org.geotools.data.collection ListFeatureCollection]
    [org.geotools.feature.simple SimpleFeatureBuilder]
    [org.geotools.geojson.feature FeatureJSON]
    [org.geotools.geojson.geom GeometryJSON]))

;;https://stackoverflow.com/questions/34934948/leaflet-and-geojson#34935345
(defn connections>geojson
  [connections]
  ;connections)
  {:type "FeatureCollection"
   :features [(let [x (first connections)]
                {:type "Feature"
                 :geometry (:geometry x)
                 :properties {:id (:id x)
                               :name (:distname x)}})]})

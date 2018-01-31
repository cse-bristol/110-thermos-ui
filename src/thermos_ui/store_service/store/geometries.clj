(ns thermos-ui.store-service.store.geometries
  (:require [clojure.java.jdbc :as j]
            [clojure.string :refer [join]]
            [environ.core :refer [env]]))

;;TODO Spec for zoom level
;;TODO Spec for lat lon?
(def pg-db {:dbtype "postgresql"
            :dbname (env :pg-db-geometries)
            :host (env :pg-host)
            :user (env :pg-user)
            :password (env :pg-password)})

;;18/130993/87111.png
(defn create-bounding-box
  [zoom x-tile y-tile]
  (let [n (Math/pow 2 zoom)
        to-lon-lat (fn [x y n]
                     (let [lon-deg (- (* (/ x n) 360) 180)
                          lat-rad (Math/atan
                                   (Math/sinh
                                    (* (Math/PI)
                                       (- 1
                                          (* 2 (/ y n))))))
                          lat-deg (Math/toDegrees lat-rad)]
                       [lon-deg lat-deg]))
        points [(to-lon-lat x-tile y-tile n)
                (to-lon-lat x-tile (+ 1 y-tile) n)
                (to-lon-lat (+ 1 x-tile) (+ 1 y-tile) n)
                (to-lon-lat (+ 1 x-tile) y-tile n)
                (to-lon-lat x-tile y-tile n)]]
    {:points points
     :geom-string (str "POLYGON(("
                       (join "," (map (fn [ps] (join " " ps)) points))
                       "))")}))
                                 
;; - https://github.com/cse-bristol/110-thermos-heat-mapping/blob/master/osm-to-addressbase/src/thermos/data/io.clj
(defn get-connections
  "x and y are tile co-ordinates z is the zoom level"
  [z x y]
  (let [bb (create-bounding-box z x y)
        query (str "SELECT  id, distname, roadnumber, classification, demand_id, nodes, ST_AsGeoJSON(geom) as geometry FROM connections "
                   "WHERE connections.geom && ST_GeomFromText('" (:geom-string bb) "')")
        results (j/query pg-db query)]
    results))

;;query = "SELECT ST_AsMVT(tile) FROM (SELECT id, name, ST_AsMVTGeom(geom, ST_Makebox2d(ST_transform(ST_SetSrid(ST_MakePoint(%s,%s),4326),3857),ST_transform(ST_SetSrid(ST_MakePoint(%s,%s),4326),3857)), 4096, 0, false) AS geom FROM admin_areas) AS tile"

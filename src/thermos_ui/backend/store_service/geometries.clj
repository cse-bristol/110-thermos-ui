(ns thermos-ui.backend.store-service.geometries
  (:require [clojure.java.jdbc :as j]
            [clojure.string :refer [join]]
            [thermos-ui.backend.config :refer [config]]))

;;TODO Spec for zoom level
;;TODO Spec for lat lon?
(def pg-db {:dbtype "postgresql"
            :dbname (config :pg-db-geometries)
            :host (config :pg-host)
            :user (config :pg-user)
            :password (config :pg-password)})

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
                                 
(defn get-candidates [z x y]
  (let [bb (create-bounding-box z x y)
        bb (:geom-string bb)
        query (str "SELECT id, name, type, building_type, postcode, demand, ST_AsGeoJSON(geometry) as geometry FROM buildings "
                   "WHERE buildings.geometry && ST_GeomFromText('" bb "')")

        buildings (j/query pg-db query)

        query (str "SELECT id, 'path' as type, name, postcode, length, node_from, node_to,
ST_Length(geometry) as length,
ST_AsGeoJSON(geometry) as geometry FROM ways "
                   "WHERE ways.geometry && ST_GeomFromText('" bb "')")
        ways (j/query pg-db query)
        ]
    (concat buildings ways)))

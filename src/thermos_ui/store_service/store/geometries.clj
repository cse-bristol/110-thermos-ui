(ns thermos-ui.store-service.store.geometries
  (:require [clojure.java.jdbc :as j]
            [clojure.string :refer [join]]
            [environ.core :refer [env]]))

;;TODO Spec for zoom level
;;TODO Spec for lat lon?

(defonce geo-ssid 4326)
(def pg-db {:dbtype "postgresql"
            :dbname (env :pg-db-geometries)
            :host (env :pg-host)
            :user (env :pg-user)
            :password (env :pg-password)})

(defonce res-matrix
  ;;Matrix source https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Resolution_and_Scale
  ;;key is os zoom level, r = m/px, s = scale at 96dpi 
  ;;Yes I know this could now be a vector...
  {0   {:r 156543.03   :s-96-dpi "1:554 678 932"}
   1   {:r 78271.52    :s-96-dpi "1:277 339 466"}
   2   {:r 39135.76    :s-96-dpi "1:138 669 733"}
   3   {:r 19567.88    :s-96-dpi "1:69 334 866" }
   4   {:r 9783.94     :s-96-dpi "1:34 667 433" }
   5   {:r 4891.97     :s-96-dpi "1:17 333 716" }
   6   {:r 2445.98     :s-96-dpi "1:8 666 858"  }
   7   {:r 1222.99     :s-96-dpi "1:4 333 429"  }
   8   {:r 611.50      :s-96-dpi "1:2 166 714"  }
   9   {:r 305.75      :s-96-dpi "1:1 083 357"  }
   10  {:r 152.87      :s-96-dpi "1:541"        }
   11  {:r 76.437      :s-96-dpi "1:270"        }
   12  {:r 38.219      :s-96-dpi "1:135"        }
   13  {:r 19.109      :s-96-dpi "1:67 709"     }
   14  {:r 9.5546      :s-96-dpi "1:33 854"     }
   15  {:r 4.7773      :s-96-dpi "1:16 927"     }
   16  {:r 2.3887      :s-96-dpi "1:8 463"      }
   17  {:r 1.1943      :s-96-dpi "1:4 231"      }
   18  {:r 0.5972      :s-96-dpi "1:2 115"      }})

(defn create-bounding-box
  "Create points for bounding box from lat,long and openstreet map zoom level, probaly only works for zooms > 4"
  ;;Anwser from stack overflow questions/7477003
  [lat lon z]
  (let [p-string (fn [x y] (join " " [x y]))
        r-earth-km 6378
        distance (:r (get res-matrix z))
        kms-per-deg (* (/ distance r-earth-km)
                       (/ 180 Math/PI))
        new-lat (+ lat kms-per-deg)
        new-long (+ lon (/ kms-per-deg
                           (Math/cos (* lat
                                        (/ Math/PI 180)))))
        points [[lat lon]
                [new-lat lon]
                [new-lat new-long]
                [lat new-long]
                [lat lon]]]

    {:points points
     :geom-string (str "POLYGON(("
                       (join "," [(p-string lat lon)
                                  (p-string new-lat lon)
                                  (p-string new-lat new-long)
                                  (p-string lat new-long)
                                  (p-string lat lon)]) 
                       "))")}))

;;18/130993/87111.png
(defn create-bounding-box-from-tile-numbers
  [x-tile y-tile zoom]
  (let [n (Math/pow 2 zoom)
        to-lon-lat (fn [x y n]
                     (let [lon-deg (- (* (/ x n) 360) 180)
                          lat-rad (Math/atan
                                   (Math/sinh
                                    (* (Math/PI)
                                       (- 1
                                          (* 2 (/ y n))))))
                          lat-deg (Math/toDegrees lat-rad)]
                       [lat-deg lon-deg]))]
        {:geom-string (str "POLYGON(("
                           (join "," [(to-lon-lat x-tile y-tile n)
                                      (to-lon-lat x-tile (+ 1 y-tile) n)])
                           "))")}))
                                 
        
        

;;TODO Get into correct GeoJsonFormat a la the current geonjson files we have... 
;; - https://github.com/cse-bristol/110-thermos-heat-mapping/blob/master/osm-to-addressbase/src/thermos/data/io.clj
;;TODO I expect the ssid is incorrect, so make sure we get the geonjson's into postgress correctly
(defn get-buildings
  "x and y are lat/lng z is the zoom level"
  [x y z]
  (let [bb (create-bounding-box x y z)
        query (str "SELECT  ST_AsGeoJSON(geom) FROM connections "
                   "WHERE connections.geom && ST_GeomFromText('" (:geom-string bb) "'," geo-ssid ")")
        results (j/query pg-db query)]
    results))

;;query = "SELECT ST_AsMVT(tile) FROM (SELECT id, name, ST_AsMVTGeom(geom, ST_Makebox2d(ST_transform(ST_SetSrid(ST_MakePoint(%s,%s),4326),3857),ST_transform(ST_SetSrid(ST_MakePoint(%s,%s),4326),3857)), 4096, 0, false) AS geom FROM admin_areas) AS tile"

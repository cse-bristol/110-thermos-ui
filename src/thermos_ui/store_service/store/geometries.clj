(ns thermos-ui.store-service.store.geometries
  (:require [clojure.java.jdbc :as j]
            [environ.core :refer [env]]))

;;TODO Spec for zoom level
;;TODO Spec for lat lon?

(def pg-db {:dbtype "postgresql"
            :dbname (env :pg-db-geometries)
            :host (env :pg-host)
            :user (env :pg-user)
            :password (env :pg-password)})

(defonce res-matrix
  ;;Matrix source https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Resolution_and_Scale
  ;;key is os zoom level, r = m/px, s = scale at 96dpi 
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
  (let [r-earth-km 6378
        distance (:r (get res-matrix z))
        kms-per-deg (* (/ distance r-earth-km)
                       (/ 180 Math/PI))
        new-lat (+ lat kms-per-deg)
        new-long (+ lon (/ kms-per-deg
                           (Math/cos (* lat
                                        (/ Math/PI 180)))))]
    [[lat lon]
     [new-lat lon]
     [new-lat new-long]
     [lat new-long]
     [lat lon]]))

(defn get-buildings
  "x and y are lat/lng z is the zoom level"
  [x y z]
  (j/query pg-db
   ["select * from buildings"]))

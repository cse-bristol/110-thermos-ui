(ns thermos-backend.importer.cooling
  "Cooling demand model.

  @Article{RePEc:eee:energy:v:110:y:2016:i:c:p:148-156,
  author={Werner, Sven},
  title={{European space cooling demands}},
  journal={Energy},
  year=2016,
  volume={110},
  number={C},
  pages={148-156},
  month={},
  keywords={Space cooling; District cooling; Modelling; Planning},
  doi={10.1016/j.energy.2015.11.},
  abstract={Information about European space cooling demands is rare, since cooling demands are not properly measured, when electricity is used for operating space cooling devices. Cooling demands are only measured at deliveries from district cooling systems. However, information about cooling demands by location and country is required for planning district cooling systems and modelling national energy systems. In order to solve this cooling information dilemma, space cooling demands have been assessed for European service sector buildings. These estimations were based on cold deliveries from twenty different European district cooling locations in eight countries. Main findings are that (1) the estimated specific cold deliveries are somewhat lower than other estimations based on electricity inputs and assumed performance ratios, (2) aggregated space cooling demands are presented by country, and (3) an European contour map is presented for average specific space cooling demands for service sector buildings.},
  url={https://ideas.repec.org/a/eee/energy/v110y2016icp148-156.html}
  }

  Also BSRIA Rules of thumb 5th edition page 52
  ISBN 978 0 86022 692 5
  "
  (:require [cljts.core :as jts]
            [thermos-importer.geoio :as geoio]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [thermos-util :refer [annual-kwh->kw]]))

(def eci-shapes
  "This and other things in this namespace are (delay)ed; this
  is because they aren't AOT compileable, because something is wrong
  with the build classpath's access to geotools.

  I'll fix it later."
  (delay (with-open [r (io/reader (io/resource "thermos_backend/importer/eci-epsg.json"))]
           (geoio/read-from-geojson-2 r))))

(def mean-eci
  (delay
    (double
     (/ (reduce + 0 (map :Index (::geoio/features @eci-shapes)))
        (count @eci-shapes)))))

(def xform (delay (geoio/reprojector "EPSG:4326" (::geoio/crs @eci-shapes))))

(defn get-eci [lon lat]
  (let [point (::geoio/geometry (@xform {::geoio/geometry (jts/create-point lon lat)}))
        matches (filter
                 #(.contains (::geoio/geometry %) point)
                 (::geoio/features @eci-shapes))]
    (double
     (if (empty? matches)
       (let [shapes (for [shape (::geoio/features @eci-shapes)]
                       (assoc shape :distance
                              (jts/distance-between
                               point (::geoio/geometry shape)
                               :geodesic true)))
             best-match (first (sort-by :distance shapes))]
         (if (< (/ (:distance best-match) 1000.0) 50.0) ;; allow 50KM discrepancy
           (do
             (log/warn "Using nearby ECI at" (:distance best-match) "km")
             (:Index best-match))
           (do
             (log/warn "No ECI at or near" lon lat "so producing zero")
             0)))
       
       (:Index (first matches))))))

(defn cooling-benchmark [lon lat]
  (let [eci (get-eci lon lat)]
    (max 0 (- (* 1.22 eci) 43.5))))

(let [uk-kw-per-m2-avg (delay (annual-kwh->kw (cooling-benchmark -0.118092 51.5)))
      uk-kw-per-m2-resi (/ 70.0 1000.0)
      uk-kw-per-m2-nonresi (/ 150.0 1000.0)]
  (def resi-cooling-kwp-per-kw (delay (/ uk-kw-per-m2-resi @uk-kw-per-m2-avg)))
  (def nonresi-cooling-kwp-per-kw (delay (/ uk-kw-per-m2-nonresi @uk-kw-per-m2-avg))))

(defn cooling-demand [lon lat floor-area]
  (* (cooling-benchmark lon lat) floor-area))

(defn cooling-peak [cooling-demand resi]
  (* (if resi
       @resi-cooling-kwp-per-kw
       @nonresi-cooling-kwp-per-kw)
     (annual-kwh->kw cooling-demand)))

;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.importer.heat-degree-days
  "Heat degree days model."
  (:require [cljts.core :as jts]
            [thermos-importer.geoio :as geoio]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]))

(def hdd-shapes
  (with-open [r (io/reader (io/resource "thermos_backend/importer/hdd.json"))]
           (geoio/read-from-geojson-2 r)))

(defn get-hdd [lon lat]
  (let [point (jts/create-point lon lat)
        matches (filter
                 #(.contains (::geoio/geometry %) point)
                 (::geoio/features hdd-shapes))]
    (double
     (if (empty? matches)
       (let [shapes (for [shape (::geoio/features hdd-shapes)]
                      (assoc shape :distance
                             (jts/distance-between
                              point (::geoio/geometry shape)
                              :geodesic true)))
             best-match (first (sort-by :distance shapes))]
         (if (< (/ (:distance best-match) 1000.0) 75.0) ;; allow 75KM discrepancy
           (do
             (log/warn "Using nearby HDD at" (:distance best-match) "m")
             (:hdd best-match))
           (do
             (log/warn "No HDD at or near" lon lat "so producing -1")
             -1)))

       (:hdd (first matches))))))

;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-util.hot-water
  (:require [thermos-util.pwlin :refer [linear-evaluate]]))

(def heat-capacity 4.18)

(def ^:private density-curve
  "Taken from https://en.wikipedia.org/wiki/Water_%28data_page%29"
  [[0.0   0.9998395]
   [3.984 0.999972]
   [4.0   0.9999720]
   [5.0   0.99996]
   [10.0  0.9997026]
   [15.0  0.9991026]
   [20.0  0.9982071]
   [22.0  0.9977735]
   [25.0  0.9970479]
   [30.0  0.9956502]
   [35.0  0.99403]
   [40.0  0.99221]
   [45.0  0.99022]
   [50.0  0.98804]
   [55.0  0.98570]
   [60.0  0.98321]
   [65.0  0.98056]
   [70.0  0.97778]
   [75.0  0.97486]
   [80.0  0.97180]
   [85.0  0.96862]
   [90.0  0.96531]
   [95.0  0.96189]
   [100.0 0.95835]])

(defn water-density
  "What is the density of water at the given temperature"
  ^double [^double t]
  (* 1000 (linear-evaluate density-curve t)))

(defn heat-loss-w-per-m
  "For a normal hot water pipe, how much heat loss do we expect"
  ^double [^double delta-t ^double diameter]
  (* delta-t
     (if (zero? diameter) 0 (+ (* 0.16807 (Math/log diameter)) 0.85684))))

(defn kw-per-m
  "For a hot water pipe, how many kw can we carry with a given diameter
  in metres?"
  ^double
  [^double diameter
   ^double delta-t
   ^double t-avg]
  (let [density   (water-density t-avg)
        area      (* Math/PI (Math/pow (* diameter 0.5) 2.0))
        velocity  (- (* 4.7617 (Math/pow diameter 0.3701))
                    0.4834)
        flow-rate (* area velocity)
        mass-rate (* flow-rate density)]
    (* mass-rate heat-capacity delta-t)))


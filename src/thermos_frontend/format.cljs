;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.format
  (:require [thermos-util :as util]))

(let [format (js/Intl.NumberFormat.
              js/undefined
              #js {"maximumFractionDigits" 2}
              )]
  (defn local-format [value] (.format format value)))

(defn metric-prefix [value scale prefix]
  (when (>= value scale)
    (str (local-format (/ value scale)) " " prefix)))

(defn si-number [value & [dimension]]
  (and value
       (let [sign (if (< value 0) "-" )
             value (Math/abs value)
             ]
         (if dimension
           (let [scale (case (aget dimension 0)
                         "m" 0.001
                         "c" 0.01
                         "k" 1000
                         "M" 1000000
                         "G" 1000000000
                         "T" 1000000000000)]
             (str sign
                  (or (metric-prefix value (/ 1000000000000 scale) "T")
                      (metric-prefix value (/ 1000000000 scale) "G")
                      (metric-prefix value (/ 1000000 scale) "M")
                      (metric-prefix value (/ 1000 scale) "k")
                      (metric-prefix value scale "")
                      (metric-prefix value (/ 0.001 scale) "m")
                      (local-format value))
                  (.substring dimension 1)))
           (str sign
                (or (metric-prefix value 1000000000000 "T")
                    (metric-prefix value 1000000000 "G")
                    (metric-prefix value 1000000 "M")
                    (metric-prefix value 1000 "k")
                    (metric-prefix value 1 "")
                    (metric-prefix value 0.001 "m")
                    (local-format value)))))))

(def seconds util/format-seconds)
(def scale
  {"T" 1000000000000
   "G" 1000000000
   "M" 1000000
   "k" 1000
   "m" 0.001})

(defn parse-si-number [t]
  (try
    (let [n (js/parseFloat t)
          s (last t)]
      (and (js/isFinite n)
           (if-let [scale (scale s)]
             (* n scale)
             n)))
    (catch :default e nil)))

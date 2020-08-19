(ns thermos-util.steam
  (:require [clojure.string :as string]
            [thermos-util :refer [assoc-by]])
  #?(:cljs (:require-macros [thermos-util.steam :refer [load-steam-table]])))

;; steam table data is loaded by a macro at compile time
#?(:clj
   (defmacro load-steam-table []
     (let [table (with-open
                   [reader
                    (clojure.java.io/reader
                     (clojure.java.io/resource "thermos_util/steam-tables.csv"))]
                   (vec (clojure.data.csv/read-csv reader)))]
       (let [[header & rows] table
             header (map (comp keyword string/trim) (remove string/blank? header))
             rows   (for [row rows] (keep #(try (Double/parseDouble %) (catch Exception e)) row))
             ]

         (assoc-by (map (partial zipmap header) rows) :MPa))
       )))

(def steam-table (into (sorted-map) (load-steam-table)))

(defn interpolate-maps [below above p]
  (reduce-kv
   (fn [a k v]
     (assoc a k (+ v (* p (- (get above k) v)))))
   {}
   below))

(defn saturated-steam-properties [pressure]
  (let [[p above] (first (subseq steam-table >= pressure))]
    (if (= p pressure)
      above
      (let [[p2 below] (first (rsubseq steam-table < pressure))
            frac       (/ (- pressure p2) (- p p2))]
        (interpolate-maps below above frac)))))

(defn pipe-capacity [MPa m%s dia-m]
  (let [ssp      (saturated-steam-properties MPa)
        density  (/ (:vg ssp)) ;; this is kg/m3
        enthalpy (:hfg ssp)    ;; kJ/kg
        area     (* Math/PI dia-m dia-m)
        m3%s     (* area m%s)
        kg%s     (* density m3%s)
        kj%s     (* enthalpy kg%s)
        ]
    kj%s))

;; (pipe-capacity 0.8 20 0.3) for example

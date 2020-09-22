(ns thermos-util.steam
  (:require [clojure.string :as string]
            [thermos-util :refer [assoc-by parse-double]]
            [com.rpl.specter :as sr])
  #?(:cljs (:require-macros [thermos-util.steam :refer [load-csv]])))

;; steam table data is loaded by a macro at compile time

;; tables are: steam-tables.csv, which contains thermodynamic
;; properties of saturated steam

;; steam-losses.csv, which contains W/m emissions alleged in air at 20 degrees in W/m
;; for a given temperature differential.
;; I'm not sure if this should be different for pipes not in air?

#?(:clj
   (defmacro load-csv [resource-name]
     (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource resource-name))]
       (vec (clojure.data.csv/read-csv rdr)))))

(def steam-properties
  (let [[header & rows] (load-csv "thermos_util/steam-tables.csv")
        header (map (comp keyword string/trim) (remove string/blank? header))
        rows   (for [row rows] (keep parse-double row))]
    (-> (map (partial zipmap header) rows)
        (assoc-by  :MPa)
        (->> (into (sorted-map))))))

(defn interpolate-maps [below above p]
  (reduce-kv
   (fn [a k v]
     (assoc a k (+ v (* p (- (get above k) v)))))
   {}
   below))

(defn saturated-steam-properties [pressure]
  (let [[p above] (first (subseq steam-properties >= pressure))]
    (if (= p pressure)
      above
      (let [[p2 below] (first (rsubseq steam-properties < pressure))
            frac       (/ (- pressure p2) (- p p2))]
        (interpolate-maps below above frac)))))

(defn pipe-capacity-kw ^double [^double MPa ^double m%s ^double dia-m]
  (let [ssp      (saturated-steam-properties MPa)
        density  (/ (:vg ssp)) ;; this is kg/m3
        enthalpy (:hfg ssp)    ;; kJ/kg
        area     (* Math/PI dia-m dia-m)
        m3%s     (* area m%s)
        kg%s     (* density m3%s)
        kj%s     (* enthalpy kg%s)
        ]
    kj%s))

(def steam-pipe-losses
  "A bilevel sorted map. First level is delta-t, second level is
  diameter, values at the bottom are watts/m heat loss.

  Marc says heat losses calculated in terms of kg/h

  This might be more relevant if steam systems run at peak most of time?
  "
  (let [[header & rows] (load-csv "thermos_util/steam-losses.csv")
        header (map (comp keyword string/trim) header)
        rows (map #(map parse-double %) rows)
        ]
    (-> (map (partial zipmap header) rows)
        (->> (group-by :dt)
             (sr/transform
              sr/MAP-VALS
              #(into (sorted-map) (for [r %] [(:dia r) (:w r)])))
             (into (sorted-map))))))

(defn- bounds-in [sm k]
  [(first (rsubseq sm < k)) (first (subseq sm >= k))])

(defn heat-losses-kwh%m-yr ^double [^double MPa ^double dia-m]
  (let [ssp    (saturated-steam-properties MPa)
        temp   (:degC ssp)
        dia-mm (* dia-m 1000.0)
        ;; given temp we can find rows above and below in steam-pipe-losses
        basic-loss-w
        (let [[[tb below]
               [ta above]] (bounds-in steam-pipe-losses temp)

              [[dal wal]
               [dar war]] (bounds-in above dia-mm)
              
              [[_ wbl]
               [_ wbr]] (bounds-in below dia-mm)

              ;; so we need to interpolate wal <> wbl with temp
              ;; and war <> wbr with temp, and then between those with dia

              pt (/ (- temp tb) (- ta tb))
              pw (/ (- dia-mm dal) (- dar dal))

              wl (+ wbl (* (- wal wbl) pt))
              wr (+ wbr (* (- war wbr) pt))
              ]
          
          (+ wl (* pw (- wr wl))))
        ]
    basic-loss-w
    ))



(ns thermos-util.steam
  (:require [clojure.string :as string]
            [thermos-util :refer [assoc-by parse-double kw->annual-kwh]]
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
       (vec (remove
             #(-> % first (.startsWith "#"))
             (clojure.data.csv/read-csv rdr))))))

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

(defn- bounds-in
  "Given a sorted map `sm`, find the values above and below `k` in it."
  [sm k]
  [(first (rsubseq sm < k)) (first (subseq sm >= k))])

(defn into-multilevel-sorted-map
  "A recursive group-by ks on xs.
  The thing at the end of a path is (val [thing1 thing2 ...])"
  
  [xs ks val]
  (if (seq ks)
    (let [[k & ks] ks
          by-k     (group-by k xs)]
      (with-meta
        (into
         (sorted-map)
         (sr/transform
          sr/MAP-VALS
          #(into-multilevel-sorted-map % ks val)
          by-k))
        {:level k}))
    (val xs)))

(defn interpolate-multilevel-sorted-map
  "Taking `into-multilevel-sorted-map` as a multidimensional table where
  each map level is a dimension, lookup the given coordinates in that
  table, interpolating if needs be.

  Fails if OOB.
  "
  [m coords]
  (if (empty? coords)
    m
    
    (let [[c & coords] coords
          [greater-key greater-vals] (first (subseq m >= c))]
      (when (nil? greater-key)
        (throw
         (ex-info
          "No upper bound interpolating multilevel map"
          (assoc (meta m)
                 :value c
                 :greatest-key (reduce max (keys m))))))
      
      (if (or (== greater-key c)
              (< (Math/abs (- greater-key c)) 0.001))
        (interpolate-multilevel-sorted-map greater-vals coords)
        (let [[lesser-key lesser-vals] (first (rsubseq m < c))]
          (when (nil? lesser-key)
            (throw
             (ex-info
              "No lower bound interpolating multilevel map"
              (assoc (meta m)
                     :value c
                     :least-key (reduce min (keys m))))))
            
          (if (< (Math/abs (- lesser-key c)) 0.001)
            (interpolate-multilevel-sorted-map lesser-vals coords)

            ;; only interpolate here
            (let [lesser-val
                  (interpolate-multilevel-sorted-map lesser-vals coords)

                  greater-val
                  (interpolate-multilevel-sorted-map greater-vals coords)

                  share
                  (/ (- c lesser-key) (- greater-key lesser-key))]
              (+ (* share lesser-val)
                 (* (- 1.0 share) greater-val))
              ))
          )))))

(def steam-pipe-losses
  "A bilevel sorted map. First level is delta-t, second level is
  diameter, values at the bottom are watts/m heat loss.

  Marc says heat losses calculated in terms of kg/h

  This might be more relevant if steam systems run at peak most of time?
  "
  (let [[header & rows] (load-csv "thermos_util/steam-losses.csv")
        header          (map (comp keyword string/trim) header)
        rows            (map #(map parse-double %) rows)
        cells           (map (partial zipmap header) rows)
        ]
    (into-multilevel-sorted-map
     cells
     [:dt :dia]
     (comp :w first))))

(def steam-pipe-insulation-factors
  "{thickness => {pressure (bar g) => {NB (mm) => loss factor}}}

   The loss factor is multiplied with value interpolated from `steam-pipe-losses`.
   
   This is a map from thickness to a sorted map of sorted maps."

  (let [[header & rows] (load-csv "thermos_util/steam-insulation-factors.csv")
        header          (map (comp keyword string/trim) header)
        rows            (map #(map parse-double %) rows)
        cells           (map (partial zipmap header) rows)
        ]
    (into-multilevel-sorted-map
     cells
     [:ins :dia :pressure]
     (comp :factor first))))

(defn heat-losses-kwh%m-yr ^double [^double MPa ^double dia-m]
  (let [ssp    (saturated-steam-properties MPa)
        temp   (:degC ssp)
        dia-mm (* dia-m 1000.0)

        ;; given temp we can find rows above and below in steam-pipe-losses
        basic-loss-w
        (interpolate-multilevel-sorted-map
         steam-pipe-losses [temp dia-mm])

        bar-g (- (* 10 MPa) 1.0) ;; ish
        
        loss-factor
        (interpolate-multilevel-sorted-map
         steam-pipe-insulation-factors
         [50 dia-mm bar-g]
         )
        ]
    (kw->annual-kwh
     (/ (* basic-loss-w loss-factor) 1000.0))))





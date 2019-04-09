(ns thermos-specs.path
  (:require [clojure.spec.alpha :as s]))

(s/def ::path
  (s/and
   #(= (::type % :path))
   (s/keys :req [::length ::cost-per-m ::cost-per-m2 ::start ::end ])))

(s/def ::length number?)
(s/def ::cost-per-m number?)
(s/def ::cost-per-m2 number?)
(s/def ::start string?)
(s/def ::end string?)

(defn cost [path
            civil-exponent
            mechanical-fixed
            mechanical-variable
            mechanical-exponent
            diameter]
  (let [l (or (::length path) 0)
        diameter (/ diameter 1000.0) ;; to m
        civil-fixed (or (::cost-per-m path) 0)
        civil-variable (or (::cost-per-m2 path) 0)]
    (* l
       (+ civil-fixed
          mechanical-fixed
          (Math/pow (* diameter civil-variable) civil-exponent)
          (Math/pow (* diameter mechanical-variable) mechanical-exponent)))))


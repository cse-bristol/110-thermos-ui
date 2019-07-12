(ns thermos-specs.path
  (:require [clojure.spec.alpha :as s]))

(s/def ::path
  (s/and
   #(= (::type % :path))
   (s/keys :req [::length ::civil-cost-id ::start ::end ])))

(s/def ::length number?)
(s/def ::cost-per-m number?)
(s/def ::cost-per-m2 number?)
(s/def ::start string?)
(s/def ::end string?)

(defn cost [path
            civil-fixed
            civil-variable
            civil-exponent
            mechanical-fixed
            mechanical-variable
            mechanical-exponent
            diameter]
  (let [l (or (::length path) 0)
        diameter (/ diameter 1000.0) ;; to m
        civil-fixed (or civil-fixed 0)
        civil-variable (or civil-variable 0)
        civil-exponent (or civil-exponent 0)

        mechanical-fixed (or mechanical-fixed 0)
        mechanical-variable (or mechanical-variable 0)
        mechanical-exponent (or mechanical-exponent 0)]
    (* l
       (+ civil-fixed
          mechanical-fixed
          (Math/pow (* diameter civil-variable) civil-exponent)
          (Math/pow (* diameter mechanical-variable) mechanical-exponent)))))

(s/def ::civil-cost-id int?)

(s/def ::civil-cost
  (s/keys :req
          [::civil-cost-id
           ::civil-cost-name
           ::fixed-cost
           ::variable-cost]))


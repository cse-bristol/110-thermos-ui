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

(defn cost-per-m [civil-fixed
                  civil-variable
                  civil-exponent
                  mechanical-fixed
                  mechanical-variable
                  mechanical-exponent
                  diameter-mm]
  (let [diameter-m (/ diameter-mm 1000.0) ;; to m
        civil-fixed (or civil-fixed 0)
        civil-variable (or civil-variable 0)
        civil-exponent (or civil-exponent 0)

        mechanical-fixed (or mechanical-fixed 0)
        mechanical-variable (or mechanical-variable 0)
        mechanical-exponent (or mechanical-exponent 0)]
    (+ civil-fixed
       mechanical-fixed
       (Math/pow (* diameter-m civil-variable) civil-exponent)
       (Math/pow (* diameter-m mechanical-variable) mechanical-exponent))))


(defn cost [path
            civil-fixed
            civil-variable
            civil-exponent
            mechanical-fixed
            mechanical-variable
            mechanical-exponent
            diameter-mm]
  (let [l (or (::length path) 0)
        ]
    (* l (cost-per-m civil-fixed
                     civil-variable
                     civil-exponent
                     mechanical-fixed
                     mechanical-variable
                     mechanical-exponent
                     diameter-mm))))

(s/def ::civil-cost-id int?)

(s/def ::civil-cost
  (s/keys :req
          [::civil-cost-id
           ::civil-cost-name
           ::fixed-cost
           ::variable-cost]))


(ns thermos-specs.tariff
  (:require [clojure.spec.alpha :as s]
            [thermos-util :refer [annual-kwh->kw]]))

(s/def ::tariff
  (s/keys :req [::id
                ::name

                ::standing-charge
                ::unit-charge
                ::capacity-charge]))

(s/def ::connection-cost
  (s/keys :req [::cc-id
                ::name
                ::fixed-connection-cost
                ::variable-connection-cost]))


(s/def ::name string?)

(defn annual-heat-revenue [tariff demand-kwh capacity-kw]
  (let [demand-kwh (or demand-kwh 0)
        capacity-kw (or capacity-kw (annual-kwh->kw demand-kwh))]
    (+ (::standing-charge tariff 0)
       (* (::unit-charge tariff 0) demand-kwh)
       (* (::capacity-charge tariff 0) capacity-kw))))

(defn connection-cost [cc demand-kwh capacity-kw]
  (let [demand-kwh (or demand-kwh 0)
        capacity-kw (or capacity-kw (annual-kwh->kw demand-kwh))]
    (+ (::fixed-connection-cost cc 0)
       (* (::variable-connection-cost cc 0) capacity-kw))))

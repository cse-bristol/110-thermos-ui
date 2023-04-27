;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

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


(s/def
  ;; this key, if present on a candidate, defines its connection cost
  ;; as computed elsewhere. It is intended for use on sites rather
  ;; than buildings.
  ::external-connnection-cost double?)

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

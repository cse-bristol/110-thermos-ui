(ns thermos-specs.supply
  (:require [clojure.spec.alpha :as s]))

(s/def ::supply
  (s/keys :req [::capacity-kwp
                ::cost-per-kwh
                ::capex-per-kwp
                ::opex-per-kwp
                ::fixed-cost
                ::emissions]))

(defn principal [candidate capacity-kw]
  (+ (::fixed-cost candidate 0)
     (* (or capacity-kw 0)
        (::capex-per-kwp candidate 0))))

(defn opex [candidate capacity-kw]
  (* (or capacity-kw 0) (::opex-per-kwp candidate 0)))

(defn heat-cost [candidate consumption-kwh]
  (* (or consumption-kwh 0) (::cost-per-kwh candidate 0)))

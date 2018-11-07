(ns thermos-ui.specs.supply
  (:require [clojure.spec.alpha :as s]))

(s/def ::supply
  (s/keys :req [::capacity-kwp
                ::cost-per-kwh
                ::capex-per-kwp
                ::opex-per-kwp
                ::fixed-cost
                ::emissions]))

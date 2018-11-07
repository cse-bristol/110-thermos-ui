(ns thermos-ui.specs.demand
  (:require [clojure.spec.alpha :as s]))

(s/def ::demand
  (s/keys :req [::kwh ::kwp ::connection-count
                ::price ::emissions ]))

(s/def ::kwh (s/and number? pos?))
(s/def ::kwp (s/and number? pos?))
(s/def ::price (s/or :no-price nil?
                     :number-price (s/and number? pos?)))

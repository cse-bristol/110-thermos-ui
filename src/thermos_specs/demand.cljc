(ns thermos-specs.demand
  (:require [clojure.spec.alpha :as s]))

(s/def ::demand
  (s/keys :req [::kwh ::kwp ::connection-count ::emissions]))

(s/def ::kwh (s/and number? pos?))
(s/def ::kwp (s/and number? pos?))


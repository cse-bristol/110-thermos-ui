(ns thermos-specs.path
  (:require [clojure.spec.alpha :as s]))

(s/def ::path
  (s/and
   #(= (::type % :path))
   (s/keys :req [::length ::civil-cost-id ::start ::end ::maximum-diameter])))

(s/def ::length number?)
(s/def ::cost-per-m number?)
(s/def ::cost-per-m2 number?)
(s/def ::start string?)
(s/def ::end string?)
(s/def ::civil-cost-id int?)

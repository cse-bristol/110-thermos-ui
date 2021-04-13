;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

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

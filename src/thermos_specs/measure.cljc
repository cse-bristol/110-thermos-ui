;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-specs.measure
  (:require [clojure.spec.alpha :as s]))

;; specs for insulation measures

(s/def ::insulation
  (s/keys :req [::id
                ::name
                ::fixed-cost
                ::cost-per-m2
                ::maximum-effect 
                ::surface]))

(s/def ::id int?)
(s/def ::name string?)
(s/def ::fixed-cost number?)
(s/def ::cost-per-m2 number?)
(s/def ::maximum-effect number?)
(s/def ::maximum-area number?)
(s/def ::surface #{:roof :wall :floor})

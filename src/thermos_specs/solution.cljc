;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-specs.solution
  (:require [clojure.spec.alpha :as s]))

(s/def ::log string?)
(s/def ::state keyword?)
(s/def ::message string?)
(s/def ::runtime integer?)

(defn exists? [document]
  (number? (::objective document)))


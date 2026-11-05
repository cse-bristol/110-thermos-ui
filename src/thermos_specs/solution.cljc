;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-specs.solution
  (:require [clojure.spec.alpha :as s]))

(s/def ::log string?)
(s/def ::state keyword?)
(s/def ::message string?)
(s/def ::runtime integer?)
;; what a solution doesn't quite do of what was asked, as [kind message]
(s/def ::warnings (s/coll-of (s/tuple keyword? string?)))
;; the ids of required paths left out of the solution, which is one of
;; the things ::warnings can be about
(s/def ::waived-paths (s/coll-of any?))

(defn exists? [document]
  (number? (::objective document)))


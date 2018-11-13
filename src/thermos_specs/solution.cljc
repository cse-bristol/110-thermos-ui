(ns thermos-specs.solution
  (:require [clojure.spec.alpha :as s]))

(s/def ::log string?)
(s/def ::state keyword?)
(s/def ::message string?)
(s/def ::runtime integer?)

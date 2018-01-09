(ns thermos-ui.specs.technology
  (:require [clojure.spec.alpha :as s])
  )

;; Specification for technology choices. This is waiting for information from Kamal to be definite.

(s/def ::technology-id string?)

;; what do we know about a technology?
;; it has size bounds, and a price related to the size
;; it has fuel inputs and heat and electricity outputs
;; with associated efficiencies. these may depend on the size.
(s/def ::technology (s/keys :req [::id ::name ::fuel ::curve]))
(s/def ::curve (s/+ ::curve-point))
(s/def ::curve-point
  (s/keys :req [ ::size ::capital-cost ::heat-efficiency ::power-efficiency ]))

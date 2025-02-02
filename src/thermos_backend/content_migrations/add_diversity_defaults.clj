(ns thermos-backend.content-migrations.add-diversity-defaults
  (:require [thermos-specs.document :as document]))

;; ideally we would refer to the defaults, but that creates a circular dependency
(defn add-diversity-and-objective-defaults [problem]
  (assoc
   problem
   ::document/diversity-limit 0.62
   ::document/diversity-rate  1.0
   ::document/objective-scale 1.0
   ::document/objective-precision 0.0
   ::document/edge-cost-precision 0.0))

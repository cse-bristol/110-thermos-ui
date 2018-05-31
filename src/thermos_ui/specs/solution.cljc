(ns thermos-ui.specs.solution
  (:require [clojure.spec.alpha :as s]
            ))

(s/def ::solution
  (s/keys :req [ ::exists ])
  )

(s/def ::candidate (s/keys :req [::included]))

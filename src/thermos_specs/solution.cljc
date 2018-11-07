(ns thermos-specs.solution
  (:require [clojure.spec.alpha :as s]))

(s/def ::summary
  (s/keys :req [::state 
                
                ])
  )



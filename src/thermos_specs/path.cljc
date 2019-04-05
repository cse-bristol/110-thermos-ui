(ns thermos-specs.path
  (:require [clojure.spec.alpha :as s]))

(s/def ::path
  (s/and
   #(= (::type % :path))
   (s/keys :req [::length ::cost-per-m ::cost-per-m-mm ::start ::end ])))

(s/def ::length number?)
(s/def ::cost-per-m number?)
(s/def ::cost-per-m-mm number?)
(s/def ::start string?)
(s/def ::end string?)

(defn cost [path]
  (when-let [l (::length path)]
    (* l (or (::cost-per-m path) 0))))

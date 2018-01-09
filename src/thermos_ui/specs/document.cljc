(ns thermos-ui.specs.document
  (:require [clojure.spec.alpha :as s]
            [thermos-ui.specs.technology :as technology]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.specs.view :as view]
            ))

;; this is the spec for what you can see on the screen in the UI

(s/def ::document (s/keys :req [ ::candidates ::technologies ::view/view-state ]))

(defn redundant-key [key]
  (s/every (fn [[id val]] (and (map? val)
                               (= id (key val)))) :kind map? :into {}))

(s/def ::technologies
  (s/and
   (redundant-key ::candidate/technology-id)
   (s/map-of ::technology/technology-id ::technology/technology)))

(s/def ::candidates
  (s/and
   (redundant-key ::candidate/candidate-id)
   (s/map-of ::candidate/candidate-id ::candidate/candidate)))

;; TODO: referential integrity checks for technology ids in candidates

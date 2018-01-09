(ns thermos-ui.specs.document
  (:require [clojure.spec.alpha :as s]
            [thermos-ui.specs.technology :as technology]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.specs.view :as view]
            ))

;; this is the spec for what you can see on the screen in the UI

(s/def ::document (s/keys :req [ ::candidates ::technologies ::view/view-state ]))

(defn redundant-key
  "Make a spec which checks a map, so that for every map entry, the
  value of the entry is also map, which contains the entry's key under
  the KEY argument, so e.g.

  (redundant-key :x) would pass {:y {:x :y}} but would fail {:y
  {:x :z}} because :z is not :y
  "
  [key]
  (s/every (fn [[id val]] (and (map? val)
                               (= id (key val)))) :kind map? :into {}))

(s/def ::technologies
  (s/and
   (redundant-key ::technology/technology-id)
   (s/map-of ::technology/technology-id ::technology/technology)))

(s/def ::candidates
  (s/and
   (redundant-key ::candidate/id)
   (s/map-of ::candidate/id ::candidate/candidate)))

;; TODO: referential integrity checks for technology ids in candidates

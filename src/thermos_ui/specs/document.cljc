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
   (redundant-key ::technology/id)
   (s/map-of ::technology/id ::technology/technology)))

(defn is-topologically-valid
  "A candidate set is topologically valid when every path connects only to junctions or buildings.
This means that anything with type :path has suitable path-start and path-end"
  [candidates]

  (let [paths
        (filter
         #(= (::candidate/type %) :path)
         (vals candidates))

        path-ids
        (into #{} (map ::candidate/id paths))

        endpoints
        (->> paths
             (mapcat #(vector (::candidate/path-start %)
                              (::candidate/path-end %)))
             (into #{}))
        ]

    ;; every endpoint must be a building ID or a junction ID
    ;; but this is always true because junction IDs may be anything

    ;; so the only real rule is that no endpoint may be a path ID:
    (and (every? (comp not path-ids) endpoints)
         ;; and also that no path may be a loop, as that would be silly
         (every? #(not= (::candidate/path-start %)
                        (::candidate/path-end %))
                 paths))
    ))


(s/def ::candidates
  (s/and
   (redundant-key ::candidate/id)
   is-topologically-valid
   (s/map-of ::candidate/id ::candidate/candidate)))

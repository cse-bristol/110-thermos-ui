(ns thermos-specs.document
  (:require [clojure.spec.alpha :as s]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.path :as path]
            [thermos-specs.solution :as solution]
            [thermos-specs.view :as view]
            [clojure.string :as str]
            [clojure.walk :refer [prewalk]]))

(s/def ::document
  (s/keys :req [::candidates
                ::view/view-state

                ;; default values for price & emissions at demands
                ::demand/price
                ::demand/emissions

                ::npv-term
                ::npv-rate

                ::loan-term
                ::loan-rate

                ::emissions-cost  ;; numbers
                ::emissions-limit ;; {:enabled and :value}
                
                ::mip-gap

                ::flow-temperature
                ::return-temperature
                ::ground-temperature

                ::mechanical-cost-per-m
                ::mechanical-cost-per-m2

                ::mechanical-cost-exponent
                ::civil-cost-exponent
                ]
          :opt [ ::solution/summary ]))

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

;; (defn is-topologically-valid
;;   "A candidate set is topologically valid when every path connects only to junctions or buildings.
;; This means that anything with type :path has suitable path-start and path-end"
;;   [candidates]

;;   (let [paths
;;         (filter
;;          #(= (::candidate/type %) :path)
;;          (vals candidates))

;;         path-ids
;;         (into #{} (mapcat ::candidate/connections paths))

;;         endpoints
;;         (->> paths
;;              (mapcat #(vector (::candidate/path-start %)
;;                               (::candidate/path-end %)))
;;              (into #{}))
;;         ]

;;     ;; every endpoint must be a building ID or a junction ID
;;     ;; but this is always true because junction IDs may be anything

;;     ;; so the only real rule is that no endpoint may be a path ID:
;;     (and (every? (comp not path-ids) endpoints)
;;          ;; and also that no path may be a loop, as that would be silly
;;          (every? #(not= (::candidate/path-start %)
;;                         (::candidate/path-end %))
;;                  paths))
;;     ))


(s/def ::candidates
  (s/and
   (redundant-key ::candidate/id)
   ;;   is-topologically-valid
   (s/map-of ::candidate/id ::candidate/candidate)))

(defn keep-interesting
  "Return a version of document in which only the interesting bits are retained.
  This strips off anything which is not part of one of the specs."
  [document]

  (let [filter-forbidden
        (fn [candidates]
          (into {}
                (filter (fn [[_ cand]]
                          (#{:optional :required} (::candidate/inclusion cand)))
                        candidates)))

        document
        (update-in document [::candidates] filter-forbidden)

        spec-key?
        (fn [x]
          (or (not (keyword? x))
              (not (namespace x))
              (str/starts-with? (namespace x) "thermos-specs")))

        remove-nonspec-keys
        (fn [x]
          (if (map? x)
            (select-keys x (filter spec-key? (keys x)))
            x))
        ]

    (prewalk remove-nonspec-keys document)))

(defn map-candidates
  "Go through a document and apply f to all the indicated candidates."
  ([doc f]
   (update doc ::candidates
           #(reduce-kv
             (fn [m k v] (assoc m k (f v))) {} %)))

  ([doc f ids]
   (if (empty? ids)
     doc
     (update doc
             ::candidates
             #(persistent!
               (reduce
                (fn [cands id]
                  (assoc! cands id (f (get cands id))))
                (transient %) ids))))))

(let [solution-ns (namespace ::solution/included)
      is-solution-keyword #(= (namespace %) solution-ns)]
  (defn remove-solution
    "Remove everything to do with a solution from this document"
    [doc]
    (-> doc
        (dissoc (filter is-solution-keyword (keys doc)))
        (map-candidates #(select-keys % (remove is-solution-keyword (keys %)))))))

(defn has-solution? [document]
  (contains? document ::solution/state))


(defn path-cost [path document]
  (path/cost
   path
   (::civil-cost-exponent document)
   (::mechanical-cost-per-m document)
   (::mechanical-cost-per-m2 document)
   (::mechanical-cost-exponent document)
   50))


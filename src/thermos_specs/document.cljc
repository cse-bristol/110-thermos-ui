(ns thermos-specs.document
  (:require [clojure.spec.alpha :as s]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.path :as path]
            [thermos-specs.solution :as solution]
            [thermos-specs.view :as view]
            [thermos-specs.measure :as measure]
            [thermos-specs.supply :as supply]
            [thermos-specs.tariff :as tariff]
            [clojure.string :as str]
            [clojure.walk :refer [prewalk]]
            [com.rpl.specter :as sr :refer-macros [transform setval]]
            [clojure.test :as test]))

(s/def ::document
  (s/keys :req [::candidates
                ::view/view-state

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

                ::tariffs

                ::civil-costs

                ::insulation
                ::alternatives

                ::maximum-supply-sites

                ::maximum-pipe-diameter
                ::minimum-pipe-diameter

                ::pumping-overhead
                ::pumping-cost-per-kwh
                ::pumping-emissions
                ]
          :opt [::solution/summary
                ::deletions]))

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

(s/def ::deletions (s/* ::candidate/id))

(s/def ::tariffs
  (s/and (redundant-key ::tariff/id)
         (s/map-of ::tariff/id ::tariff/tariff)))

(s/def ::civil-costs
  (s/and (redundant-key ::path/civil-cost-id)
         (s/map-of ::path/civil-cost-id ::path/civil-cost)))

(s/def ::candidates
  (s/and
   (redundant-key ::candidate/id)
   (s/map-of ::candidate/id ::candidate/candidate)))

(defn- is-interesting? [candidate]
  (or (::candidate/modified candidate)
      (#{:optional :required} (::candidate/inclusion candidate))))

(defn- keep-interesting-candidates
  "Given a document, remove any candidates which are not either ::candidate/modified,
  or having ::candidate/inclusion :optional or :required"
  [doc]
  {:test #(test/is
           (= (set (keys (::candidates
                          (keep-interesting-candidates
                           {::candidates {"one" {::candidate/inclusion :optional}
                                          "two" {::candidate/inclusion :forbidden
                                                 ::candidate/modified true}
                                          "three" {}
                                          "four" {::candidate/inclusion :forbidden}}}
                           ))))
              #{"one" "two"}))}
  (->> doc
       (sr/setval
        [::candidates sr/MAP-VALS
         (sr/not-selected?
          (some-fn
           ::candidate/modified
           (comp #{:optional :required} ::candidate/inclusion)))]
        sr/NONE)))

(defn- is-transient-key?
  "A key is transient if it's namespaced and not within thermos-specs."
  [x]
  {:test #(test/is (and (is-transient-key? :some.ns/kw)
                        (not (is-transient-key? :some-kw))
                        (not (is-transient-key? ::some-kw))
                        (not (is-transient-key? 1))
                        (not (is-transient-key? "str"))))}
  (and (keyword? x)
       (namespace x)
       (not (str/starts-with? (namespace x) "thermos-specs"))))

(def ALL-MAPS
  "A specter navigator that navigates to all the maps in a structure."
  (sr/recursive-path
   [] p
   (sr/cond-path
    map?
    (sr/continue-then-stay sr/MAP-VALS p)

    coll?
    [sr/ALL p]
    )))

(defn- remove-transient-keys
  "Some values are stored in the document which we don't want to persist.
  They are all stored in maps under namespaced keywors which are not
  the thermos-specs namespace or below.

  This function removes all these keys"
  [doc]
  {:test #(test/is
           (and
            (= (remove-transient-keys {}) {})
            (= (remove-transient-keys {::keep-this 1}) {::keep-this 1})
            (= (remove-transient-keys {::keep-this 1
                                       :keep-this-also 2
                                       :but-remove/this 3})
               {::keep-this 1 :keep-this-also 2})
            (= (remove-transient-keys
                {::recursively {::keep-this 1
                                :keep-this-also 2
                                :but-remove/this 3}})
               {::recursively {::keep-this 1 :keep-this-also 2}})
            (= (remove-transient-keys
                {::in-collections [{:remove/this 1} {:remove/that 2}]})
               {::in-collections [{} {}]})))}
  
  (->> doc
       (sr/setval
        [ALL-MAPS sr/MAP-KEYS is-transient-key?]
        sr/NONE)))

(defn keep-interesting
  "Return a version of document in which only the interesting bits are retained.
  This strips off anything which is not part of one of the specs."
  [document]

  (->> document
       (keep-interesting-candidates)
       (remove-transient-keys)))

(defn map-candidates
  "Go through a document and apply f to all the indicated candidates."
  ([doc f]
   (sr/transform
    [::candidates sr/MAP-VALS]
    f
    doc))

  ([doc f ids]
   (if (empty? ids)
     doc
     (sr/transform
      [::candidates (sr/submap ids) sr/MAP-VALS]
      f
      doc)
     )))

(defn map-buildings [doc f]
  (sr/transform
   [::candidates sr/MAP-VALS (sr/selected? candidate/is-building?)]
   f doc))

(defn map-paths [doc f]
  (sr/transform
   [::candidates sr/MAP-VALS (sr/selected? candidate/is-path?)]
   f doc))

(defn candidates-by-type
  "Return candidates from the given document, grouped by type"
  [doc]
  (group-by ::candidate/type (vals (::candidates doc))))

(let [solution-ns (namespace ::solution/included)
      is-solution-keyword #(and (keyword? %)
                                (= (namespace %) solution-ns))]
  (defn remove-solution
    "Remove everything to do with a solution from this document"
    [doc]
    (-> doc
        (dissoc (filter is-solution-keyword (keys doc)))
        (map-candidates #(select-keys % (remove is-solution-keyword (keys %)))))))

(defn has-solution? [document]
  (contains? document ::solution/state))

(defn civil-cost-for-id [doc cost-id]
  (let [costs (::civil-costs doc)]
    (or (get costs cost-id)
        (when-let [cost-keys (seq (keys costs))]
          (get costs (reduce min cost-keys))))))

(defn civil-cost-name [doc cost-id]
  (or (::path/civil-cost-name
       (civil-cost-for-id doc cost-id)
       (str "Civil cost " cost-id))
      "None"))

(defn path-cost [path document]
  (if (::path/exists path)
    0
    (let [cost (civil-cost-for-id document (::path/civil-cost-id path))]
      (path/cost
       path
       (::path/fixed-cost cost)
       (::path/variable-cost cost)
       (::civil-cost-exponent document)
       
       (::mechanical-cost-per-m document)
       (::mechanical-cost-per-m2 document)
       (::mechanical-cost-exponent document)
       10))))

(defn is-runnable?
  "Tells you if the document might be runnable.
  At the moment checks for the presence of a supply and some demands.
  "
  [document]
  (and (not (empty? (::candidates document)))
       (some (comp #{:building} ::candidate/type) (vals (::candidates document)))
       (some ::supply/capacity-kwp (vals (::candidates document)))))

(defn tariff-for-id [doc tariff-id]
  (if (= :market tariff-id)
    tariff-id
    (let [tariffs (::tariffs doc)]
      (when tariffs
        (or (get tariffs tariff-id)
            (get tariffs
                 (reduce min (keys tariffs))))))))

(defn tariff-name [doc tariff-id]
  (if (= tariff-id :market)
    "Market"
    (or (::tariff/name (tariff-for-id doc tariff-id)) "None")))

(defn remove-tariff
  {:test #(test/is
           (= {::tariffs {1 {}}
               ::candidates {1 {::tariff/id 1}
                             2 {}}}
              (remove-tariff
               {::tariffs {1 {} 2 {}}
                ::candidates {1 {::tariff/id 1}
                              2 {::tariff/id 2}}}
               2)))}
  [doc tariff-id]
  (-> doc
      (update ::tariffs dissoc tariff-id)
      (->> (sr/setval
            [::candidates sr/MAP-VALS ::tariff/id (sr/pred= tariff-id)]
            sr/NONE))))

(defn connection-cost-for-id [doc connection-cost-id]
  (let [connection-costs (::connection-costs doc)]
    (when connection-costs
      (or (get connection-costs connection-cost-id)
          (get connection-costs
               (reduce min (keys connection-costs)))))))

(defn connection-cost-name [doc connection-cost-id]
  (or (::tariff/name (connection-cost-for-id doc connection-cost-id)) "None"))

(defn remove-connection-cost
  {:test #(test/is
           (= {::connection-costs {1 {}}
               ::candidates {1 {::tariff/cc-id 1}
                             2 {}}}
              (remove-connection-cost
               {::connection-costs {1 {} 2 {}}
                ::candidates {1 {::tariff/cc-id 1}
                              2 {::tariff/cc-id 2}}}
               2)))}
  [doc connection-cost-id]
  (-> doc
      (update ::connection-costs dissoc connection-cost-id)
      (->> (sr/setval
            [::candidates sr/MAP-VALS ::tariff/cc-id (sr/pred= connection-cost-id)]
            sr/NONE
            ))))

(defn remove-civils [doc cost-id]
  (-> doc
      (update ::civil-costs dissoc cost-id)
      (->> (sr/setval
            [::candidates sr/MAP-VALS ::path/civil-cost-id (sr/pred= cost-id)]
            sr/NONE))))

(s/def ::insulation
  (s/and
   (redundant-key ::measure/id)
   (s/map-of ::measure/id ::measure/insulation)))

(s/def ::alternatives
  (s/and
   (redundant-key ::supply/id)
   (s/map-of ::supply/id ::supply/alternative)))
  
(defn alternative-for-id [doc alternative-id]
  (get (::alternatives doc) alternative-id))

(defn insulation-for-id [doc insulation-id]
  (get (::insulation doc) insulation-id))

(defn remove-alternative [doc alt-id]
  (-> doc
      (update ::alternatives dissoc alt-id)
      (->> (sr/setval
            [::candidates sr/MAP-VALS
             (sr/multi-path
              [::demand/counterfactual (sr/pred= alt-id)]
              [::demand/alternatives (sr/set-elem alt-id)])]
            sr/NONE))))

(defn remove-insulation [doc ins-id]
  (-> doc
      (update ::insulation dissoc ins-id)
      (->> (sr/setval
            [::candidates sr/MAP-VALS ::demand/insulation (sr/set-elem ins-id)]
            sr/NONE))))

(defn remove-candidates [doc candidates]
  (-> doc
      (update ::candidates #(apply dissoc % candidates))
      (update ::deletions concat candidates)))

(defn remove-candidate [doc id]
  (-> doc
      (update ::candidates dissoc id)
      (update ::deletions conj id)))

(defn add-candidates [doc candidates]
  (-> doc
      (update ::candidates
              merge (into {} (for [c candidates] [(::candidate/id c) c])))))

(defn add-candidate [doc candidate]
  (update doc ::candidates
          assoc (::candidate/id candidate) candidate))

(defn is-cooling? [doc]
  (< (::flow-temperature doc) (::return-temperature doc))
  ;; (= :cooling (::mode doc))
  )

(defn mode [doc]
  (if (is-cooling? doc)
    :cooling
    :heating))

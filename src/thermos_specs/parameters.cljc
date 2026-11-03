;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-specs.parameters
  "A parameter set is the cost and other parameters from a network
  document, without any candidates: a partial document, holding the
  keys for some `categories`, which are listed under ::categories.

  This namespace also merges parameters from one document into
  another, which is how a parameter set or an uploaded spreadsheet is
  applied to a network."
  (:require [thermos-specs.document :as document]
            [thermos-specs.supply :as supply]
            [thermos-specs.measure :as measure]
            [thermos-specs.tariff :as tariff]
            [thermos-util :refer [assoc-by assoc-id]]
            [com.rpl.specter :as S]
            [clojure.set :as set]
            [clojure.test :as test]))

(def categories
  [:objective
   :emissions
   :pumping
   :pipe-costs
   :tariffs
   :connection-costs
   :insulation
   :alternatives
   :supply-technologies
   :supply])

(def category-labels
  {:objective           "Objective & finance"
   :emissions           "Emissions"
   :pumping             "Pumping"
   :pipe-costs          "Pipe costs"
   :tariffs             "Tariffs"
   :connection-costs    "Connection costs"
   :insulation          "Insulation"
   :alternatives        "Individual systems"
   :supply-technologies "Supply types"
   :supply              "Supply model"})

(def temperature-keys
  [::document/flow-temperature
   ::document/return-temperature
   ::document/ground-temperature])

(def category-keys
  "Document keys which are copied as they are for each category. Tables
  which candidates refer to are in `category-tables`."
  {:objective (into temperature-keys
                    [::document/objective
                     ::document/consider-alternatives
                     ::document/consider-insulation
                     ::document/npv-term
                     ::document/npv-rate
                     ::document/loan-term
                     ::document/loan-rate
                     ::document/mip-gap
                     ::document/param-gap
                     ::document/maximum-runtime
                     ::document/maximum-iterations
                     ::document/solution-method
                     ::document/solver
                     ::document/no-loops
                     ::document/objective-scale
                     ::document/objective-precision
                     ::document/edge-cost-precision
                     ::document/capital-costs
                     ::document/maximum-supply-sites])
   :emissions [::document/emissions-cost ::document/emissions-limit]
   :pumping [::document/pumping-overhead ::document/pumping-emissions
             ::document/pumping-cost-per-kwh]
   :pipe-costs [::document/diversity-limit ::document/diversity-rate]
   :tariffs [::tariff/market-term
             ::tariff/market-discount-rate
             ::tariff/market-stickiness]
   ;; ::supply/heat-profiles is in `category-tables`
   :supply [::supply/day-types
            ::supply/plants
            ::supply/storages
            ::supply/substations
            ::supply/fuels
            ::supply/objective
            ::supply/grid-offer
            ::supply/default-profile]})

(def category-tables
  "Tables of things which candidates refer to by ID, which are merged by
  name so that the references still work."
  {:tariffs             {:table ::document/tariffs :name ::tariff/name
                         :id ::tariff/id :remove document/remove-tariff}
   :connection-costs    {:table ::document/connection-costs :name ::tariff/name
                         :id ::tariff/cc-id :remove document/remove-connection-cost}
   :insulation          {:table ::document/insulation :name ::measure/name
                         :id ::measure/id :remove document/remove-insulation}
   :alternatives        {:table ::document/alternatives :name ::supply/name
                         :id ::supply/id :remove document/remove-alternative}
   :supply-technologies {:table ::document/supply-technologies :name ::supply/name
                         :id ::supply/id :remove document/remove-supply-technology}
   :supply              {:table ::supply/heat-profiles :name :name
                         :remove supply/remove-profile}})

(defn keys-for-category [category]
  (cond-> (vec (category-keys category))
    (= :pipe-costs category) (conj ::document/pipe-costs)
    (category-tables category) (conj (:table (category-tables category)))))

(defn extract
  "The parameter set for the given `categories` of `doc`."
  [doc categories]
  (-> (select-keys doc (mapcat keys-for-category categories))
      (assoc ::categories (set categories))))

(defn categories-in
  "The categories `content` has: those it says it has, or, for something
  like a spreadsheet import, those it has any keys for."
  [content]
  (or (::categories content)
      (set (for [c categories
                 :when (some #(contains? content %) (keys-for-category c))]
             c))))

(defn merge-table
  "Lots of stuff looks like a table that goes

  {table-key
   {0 {name-key \"A thing\"}
    1 {name-key \"Another thing\"}

   ...
   ::document/candidates {0 {thing-id 1} 100 {thing-id 0} ...}}

  What we want here is to replace the table with a new one, but also
  to make sure the cross-references still work.

  All we have to join on is `name-key` (since the spreadsheet input shows
  name, not ID).

  So we have as params:
  - `join`: true if we want to preserve existing ID/name relationships
  - `merge`: true if we want to keep existing definitions whose name isn't in the new data
  - `from`: the new input data
  - `to`: the existing document we are fixing up
  - `table-key`: where the table is in the document
  - `name-key`: where the name is in a table row
  - `id-key`: where the ID is in a table row, if it is
  - `remove`: a function which takes a document and a row ID, and deletes that row from the document, including cross-refs.

  We return an updated version of `to`, in which:
  - every row in the table at `table-key` is either
    (a) found in `from`'s version of the table in a row with the same `name-key`, under the ID of `to`'s row with same name
    (b) found in `from` and not in to, with a new ID not used in the document
    (c) found in `to` and not in `from`, with the original ID, IF `merge` is true
  - All candidates and things either have no references if `join` is false, or
    have references to whatever in the table has the same name as what they joined before.
  "

  {:test #(let [to
                {:t {0 {:name "First"  :x 1}
                     1 {:name "Second" :x 2}}
                 :removed #{}}
                from
                {:t {1 {:name "First" :x 3}
                     0 {:name "Third" :x 4}}}
                rm (fn [d i] (update d :removed conj i))
                spec {:table :t :name :name :remove rm}

                no-join-no-merge
                (merge-table {:join false :merge false} from to spec)

                join-and-merge
                (merge-table {:join true :merge true} from to spec)

                join-only
                (merge-table {:join true :merge false} from to spec)

                merge-only
                (merge-table {:join false :merge true} from to spec)

                with-ids
                (merge-table {:join true :merge false}
                             {:t {1 {:id 1 :name "First" :x 3}
                                  0 {:id 0 :name "Third" :x 4}}}
                             {:t {0 {:id 0 :name "First"}
                                  1 {:id 1 :name "Second"}}}
                             (assoc spec :id :id))
                ]
            (test/is (= #{0 1} (:removed no-join-no-merge)))
            (test/is (= #{0 1} (:removed merge-only)))
            (test/is (= #{1}   (:removed join-only)))
            (test/is (= #{}    (:removed join-and-merge)))
            (test/is (= #{0 1 2} (set (keys (:t merge-only))) (set (keys (:t join-and-merge)))))
            (test/is (= #{0 1}   (set (keys (:t join-only)))))
            (test/is (-> join-and-merge :t (get 0) :x (= 3)))
            (test/is (-> join-and-merge :t (get 0) :name (= "First")))
            (test/is (-> join-and-merge :t (get 1) :name (= "Second")))
            (test/is (-> join-and-merge :t (get 2) :name (= "Third")))
            (test/is (-> join-only :t (get 1) :name (= "Third")))
            (test/is (= {0 {:id 0 :name "First" :x 3}
                         1 {:id 1 :name "Third" :x 4}}
                        (:t with-ids))))
   }

  [{:keys [join merge]}
   from to
   {table-key :table name-key :name id-key :id remove :remove}]
  (let [old-table (table-key to)
        new-table (table-key from)

        old-names (set (map name-key (vals old-table)))
        new-names (set (map name-key (vals new-table)))

        without-missing
        (->> (cond
               (not join)  (keys old-table) ;; delete all links
               (not merge) (for [[id e] old-table ;; delete names not found in input
                                 :when (not (contains? new-names (name-key e)))]
                             id)
               :else [] ;; keep everything, we will preserve IDs anyway
               )
             (reduce remove to))

        ;; so without-missing now has all the ones we don't want
        ;; deleted. Now we want to cook up a replacement table
        new-by-name (assoc-by (vals new-table) name-key)

        ;; this table preserves the old IDs, but takes values from the
        ;; new table when the names match
        replacement-table
        (->> (for [[id e] old-table
                   :let [name (name-key e)
                         new-e (get new-by-name name)]
                   :when (or merge new-e)]
               [id (or new-e e)])
             (into {}))

        ;; now we append everything from the new table which doesn't
        ;; have a matching name. We can use assoc-id to put them on
        ;; the end so the IDs don't clash.
        replacement-table
        (reduce
         (fn [out new-entry]
           (if (contains? old-names (name-key new-entry))
             out ;; do nothing, we already got it
             (assoc-id out new-entry)))
         replacement-table
         (vals new-table))

        ;; rows which say their own ID must say the one they have now
        replacement-table
        (if id-key
          (into {} (for [[id e] replacement-table] [id (assoc e id-key id)]))
          replacement-table)
        ]
    (assoc without-missing table-key replacement-table)))

(defn- merge-pipe-costs
  "Merging pipe costs is a special case because the table has two
  user-defined axes: one for civil costs, and one for diameter"
  [{:keys [merge join]} from to]
  (let [{old-civils :civils
         old-rows :rows}
        (::document/pipe-costs to)

        {new-civils :civils
         new-rows :rows
         new-default :default-civils}
        (::document/pipe-costs from)

        new-default-name (get new-civils new-default)

        new-names (set (vals new-civils))

        without-missing
        (->>
         (cond
           (not join)  (keys old-civils) ;; delete all
           (not merge) (for [[id name] old-civils
                             :when (not (contains? new-names name))]
                         id)
           :else [])
         (reduce document/remove-civils to))

        new-names (set (vals new-civils))
        old-names (set (vals old-civils))

        replacement-civils
        (->>
         (for [[id name] old-civils
               :when (or merge (contains? new-names name))]
           [id name])
         (into {}))

        replacement-civils
        (reduce
         assoc-id replacement-civils
         (remove old-names new-names))

        inv-replacement-civils (set/map-invert replacement-civils)

        remap-id
        (S/transform S/MAP-VALS inv-replacement-civils new-civils)

        new-default (get inv-replacement-civils new-default-name)

        replacement-rows
        (let [dias (if merge
                     (set/union (set (keys old-rows)) (set (keys new-rows)))
                     (set (keys new-rows)))
              ]
          (->>
           (for [dia dias]
             [(double dia)
              (let [old-row (old-rows dia)
                    new-row (new-rows dia)]
                (reduce-kv
                 (fn [row k v]
                   (let [replacement-id (or (remap-id k) k)]
                     (assoc row replacement-id v)))
                 (if merge old-row {}) new-row))])
           (into {})))

        replacement-table {:rows           replacement-rows
                           :civils         replacement-civils
                           :default-civils new-default}
        ]
    (assoc without-missing ::document/pipe-costs replacement-table)))

(defn- same-mode?
  "False if `from` and `to` both have temperatures, which say one is for
  heating and the other for cooling."
  [from to]
  (let [has-temps? #(and (number? (::document/flow-temperature %))
                         (number? (::document/return-temperature %)))]
    (or (not (has-temps? from))
        (not (has-temps? to))
        (= (document/is-cooling? from) (document/is-cooling? to)))))

(defn merge-category
  "Merge the parameters for `category` from `from` into `to`. If `from`
  has nothing for the category, `to` is unchanged."
  [opts from to category]
  (if-not (some #(contains? from %) (keys-for-category category))
    to
    (let [simple-keys (category-keys category)
          ;; never turn a heat network into a cooling network, or v.v.
          simple-keys (if (and (= :objective category) (not (same-mode? from to)))
                        (remove (set temperature-keys) simple-keys)
                        simple-keys)
          to (merge to (select-keys from simple-keys))
          table (category-tables category)]
      (cond-> to
        (and table (contains? from (:table table)))
        (as-> to (merge-table opts from to table))

        (and (= :pipe-costs category) (contains? from ::document/pipe-costs))
        (as-> to (merge-pipe-costs opts from to))))))

(defn merge-categories
  "Merge parameters from `new-state` into `old-state`. `opts` are as for
  `merge-table`, and `:categories`, those to merge."
  [old-state new-state opts]
  (reduce
   (partial merge-category opts new-state)
   old-state
   (:categories opts)))

(def apply-options
  "What we normally want when applying a parameter set: things with the
  same name are replaced, keeping their links, and things not in the
  set are removed."
  {:join true :merge false})

(defn record-source
  "Note in `doc` that it has the given `categories` from parameter set
  `source`, a map with :id, :name and :revision. Sources whose categories
  have all been replaced are forgotten."
  {:test #(let [doc (-> {}
                        (record-source {:id 1 :name "A" :revision 2} #{:tariffs})
                        (record-source {:id 2 :name "B" :revision 1} #{:tariffs :pipe-costs}))]
            (test/is (= {2 {:name "B" :revision 1 :categories #{:tariffs :pipe-costs}}}
                        (S/transform [S/MAP-VALS] (fn [x] (dissoc x :applied))
                                     (::document/parameter-sources doc)))))}
  [doc {:keys [id name revision]} categories]
  (let [categories (set categories)]
    (update doc ::document/parameter-sources
            (fn [sources]
              (-> (into {}
                        (for [[sid s] sources
                              :when (not (set/subset? (:categories s) categories))]
                          [sid s]))
                  (assoc id {:name name
                             :revision revision
                             :categories categories
                             :applied #?(:clj (java.util.Date.) :cljs (js/Date.))}))))))

(defn apply-set
  "Apply the parameter set `content` to `doc`. `opts` are for
  `merge-categories`, and :source is the set's :id, :name and :revision."
  [doc content {:keys [source] :as opts}]
  (let [opts (merge apply-options
                    {:categories (categories-in content)}
                    opts)]
    (cond-> (merge-categories doc content opts)
      source (record-source source (:categories opts)))))

(defn new-document
  "A new network document, being `base` with the parameters from
  `content` in it."
  [base content source]
  (cond-> (merge base (select-keys content
                                   (cond->> (mapcat keys-for-category (categories-in content))
                                     (not (same-mode? content base))
                                     (remove (set temperature-keys)))))
    source (record-source source (categories-in content))))

(defn complete
  "A parameter set with its missing categories filled in from `base`,
  for editing with the editor pages which expect a whole document."
  [content base]
  (merge (apply dissoc base (mapcat keys-for-category (categories-in content)))
         content))

(defn changed-sources
  "Sources in `doc` whose parameter set has a newer revision in
  `current`, a map from set id to {:revision :name}, which have not
  been dismissed."
  [doc current]
  (for [[id {:keys [revision dismissed-revision] :as s}] (::document/parameter-sources doc)
        :let [now (get current id)]
        :when (and now (> (:revision now) (max (or revision 0) (or dismissed-revision 0))))]
    (assoc s :id id :current-revision (:revision now) :name (or (:name now) (:name s)))))

(ns thermos-frontend.merge-upload
  "Dialog box for merging settings from one problem into another problem."  
  (:require [thermos-frontend.util :refer [upload-file]]
            [thermos-frontend.popover :as popover]
            [clojure.pprint]
            [thermos-util :refer [assoc-by assoc-id]]
            [reagent.core :as reagent]
            [thermos-frontend.inputs :as inputs]
            [thermos-frontend.editor-state :as state]
            [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [com.rpl.specter :as S]
            [thermos-specs.supply :as supply]
            [thermos-specs.demand :as demand]
            [thermos-specs.measure :as measure]
            [thermos-specs.tariff :as tariff]
            [thermos-specs.path :as path]
            [clojure.set :as set]
            [clojure.test :as test]
            [clojure.string :as str]))

(def categories
  [:objective
   :emissions
   :pumping
   :pipe-costs
   :alternatives
   :insulation
   :tariffs
   :connection-costs])

(def category-keys
  {:objective [::document/medium
               ::document/flow-temperature
               ::document/return-temperature
               ::document/ground-temperature
               ::document/steam-pressure
               ::document/steam-velocity
               ::document/objective
               ::document/consider-alternatives
               ::document/consider-insulation
               ::document/npv-term
               ::document/npv-rate
               ::document/loan-term
               ::document/loan-rate
               ::document/mip-gap
               ::document/param-gap
               ::document/maximum-runtime
               ::document/capital-costs
               ::document/maximum-supply-sites
               ]
   :emissions [::document/emissions-cost ::document/emissions-limit]
   :pumping [::document/pumping-overhead ::document/pumping-emissions
             ::document/pumping-cost-per-kwh]
   }
  )

(defn- merge-table
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

                no-join-no-merge
                (merge-table {:join false :merge false} from to :t :name rm)
                
                join-and-merge
                (merge-table {:join true :merge true} from to :t :name rm)

                join-only
                (merge-table {:join true :merge false} from to :t :name rm)
                
                merge-only
                (merge-table {:join false :merge true} from to :t :name rm)
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
            (test/is (-> join-only :t (get 1) :name (= "Third"))))
   }
  
  [{:keys [join merge]}
   from to
   table-key
   name-key
   remove]
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
         new-rows :rows}
        (::document/pipe-costs from)

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

        remap-id
        (S/transform S/MAP-VALS
                     (set/map-invert replacement-civils)
                     new-civils)

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
        
        replacement-table {:rows replacement-rows :civils replacement-civils}
        ]

    (assoc without-missing ::document/pipe-costs replacement-table)))


(defn- merge-category [opts from to category]
  (case category
    ;; these are simple variables in document, with no joining required
    (:objective :emissions :pumping)
    (->>
     (category-keys category)
     (select-keys from)
     (merge to))

    ;; these are joined to a specific table
    :alternatives
    (merge-table
     opts from to ::document/alternatives ::supply/name
     document/remove-alternative)
    
    :insulation
    (merge-table
     opts from to ::document/insulation ::measure/name
     document/remove-insulation)

    :tariffs
    (merge-table
     opts from to ::document/tariffs ::tariff/name
     document/remove-tariff)
    
    :connection-costs
    (merge-table
     opts from to ::document/connection-costs ::tariff/name
     document/remove-connection-cost)
    
    ;; and pipe costs is more complicated still
    :pipe-costs
    (merge-pipe-costs opts from to)
    
    to))

(defn- merge-categories [old-state new-state opts]
  (reduce
   (partial merge-category opts new-state)
   old-state
   (:categories opts)))

(defn- keyword->string [k] (str/capitalize (str/replace (name k) \- \ )))

(defn- show-errors
  ""
  ([error] 
   (cond
     (or (nil? error) (empty? error)) nil
     (map? error) [:ul {:style {:list-style :square}} (map (fn [[k v]] (show-errors k v)) error)]
     (sequential? error) [:ul {:style {:list-style :square}} (map show-errors error)]
     :else [:li {:key (str error)} (str error)]))
  ([k v] [:li {:key k} [:b (if (keyword? k) (keyword->string k) k)] ": " (show-errors v)]))

(defn- show-row-errors 
  "Special-case handling for row errors - add key with row number and remove
   any entries where there is already an error for the header."
  [row-errors cols-to-exclude]
  (show-errors
   (->> row-errors
        (map #(apply (partial dissoc %) cols-to-exclude))
        (zipmap (iterate inc 1))
        (remove (fn [[_ v]] (empty? v)))
        (map (fn [[k v]] [(str "Row " k) v]))
        (into {}))))

(defn- show-sheet-errors [[sheet errors]]
   [:li 
    {:key sheet}
    [:span "Sheet " [:b (keyword->string sheet)] ": "] 
    (cond
      (sequential? errors) (show-errors errors)
      (map? errors)
      (let [header-errors (:header errors)
            row-errors (:rows errors)]
        [:div
         (show-errors header-errors)
         (show-row-errors row-errors (keys header-errors))]))])

(defn- merge-dialog [result]
  (reagent/with-let [*state
                     (reagent/atom
                      {:categories (set categories)
                       :join true
                       :merge false})]
    (let [state @*state]
      (if (contains? result :import/errors)
        [:div.popover-dialog
         [:h2.popover-header "Failed to import spreadsheet"]
         [:button.popover-close-button
          {:on-click popover/close!}
          "⨯"]
         [:p "The spreadsheet you have chosen is not compatible with THERMOS."]
         [:b "Errors:"]
         [:div {:style {:max-height 300 :overflow-y :scroll}}
          [:ul {:style {:list-style :square}} (map show-sheet-errors (:import/errors result))]]
         
         [:div.align-right {:style {:margin-top "2em"}}
          [:button.button.button--danger
           {:on-click popover/close!
            :style {:margin-left :auto}}
           "Cancel"]]]
        
        [:div.popover-dialog
         [:h2.popover-header "Import Parameters"]
         [:button.popover-close-button
          {:on-click popover/close!}
          "⨯"]

         [:b "Categories to import:"]
         (for [category categories]
           [:div {:key category}
            [inputs/check {:label (name category)
                           :value (contains? (:categories state) category)
                           :on-change (fn [e]
                                        (swap!
                                         *state
                                         update :categories
                                         #((if (contains? % category) disj conj)
                                           % category)))}]])

         [:b "Existing parameters:"]
         [:div
          [inputs/check {:label "Use new parameters where names match"
                         :value (:join state)
                         :on-change #(swap! *state :join not)}]]
         [:div
          [inputs/check {:label "Keep old parameters with different names"
                         :value (:merge state)
                         :on-change #(swap! *state :merge not)}]]

         [:div.align-right {:style {:margin-top "2em"}}
          [:button.button.button--danger
           {:on-click popover/close!
            :style {:margin-left :auto}}
           "Cancel"]
          [:button.button
           {:on-click
            #(do (swap!
                  state/state
                  merge-categories
                  result
                  @*state)
                 (popover/close!))}
           "Apply"]]]))))

(defn- show-merge-dialog [result]
  (popover/open!
   [#'merge-dialog result]
   :middle))


(defn do-upload []
  (upload-file
   "/convert/excel"
   :accept "xlsx"
   :handler show-merge-dialog))

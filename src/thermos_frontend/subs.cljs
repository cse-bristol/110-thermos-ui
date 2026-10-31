;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.subs
  "re-frame subscriptions for the editor. Those which only need part of the
  document get it from :doc/candidates or :view/state, so they aren't
  recalculated whenever anything else in the document changes.

  Subscriptions return vectors rather than lazy sequences, as reagent
  compares each new value with the last, and that is much slower for a
  long lazy sequence."
  (:require [re-frame.core :as rf]
            [com.rpl.specter :as S]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.document :as document]
            [thermos-specs.solution :as solution]
            [thermos-specs.view :as view]
            [thermos-util :refer [count-if]]
            [thermos-util.pipes :as pipe-calcs]
            [thermos-frontend.db :as db]
            [thermos-frontend.operations :as operations]
            [thermos-frontend.spatial :as spatial]))

(rf/reg-sub
 :doc/get
 (fn [db [_ path]]
   (get-in db path)))

;; The document

(rf/reg-sub :doc/candidates (fn [db _] (::document/candidates db)))

(rf/reg-sub :doc/has-solution (fn [db _] (document/has-solution? db)))

(rf/reg-sub :doc/has-supply-solution (fn [db _] (document/has-supply-solution? db)))

(rf/reg-sub :doc/mode (fn [db _] (document/mode db)))

(rf/reg-sub :doc/pipe-costs (fn [db _] (::document/pipe-costs db)))

(rf/reg-sub :doc/pipe-parameters (fn [db _] (pipe-calcs/select-parameters db)))

(rf/reg-sub :doc/migration-messages (fn [db _] (::document/migration-messages db)))

(rf/reg-sub
 :doc/cost-factors
 ;; the settings which go into costs and descriptions of candidates
 (fn [db _]
   (dissoc db ::document/candidates ::view/view-state)))

;; Candidates and the selection

(defn- candidates-doc [candidates] {::document/candidates candidates})

(rf/reg-sub
 :candidates/included
 :<- [:doc/candidates]
 (fn [candidates _] (vec (operations/included-candidates (candidates-doc candidates)))))

(rf/reg-sub
 :candidates/selected
 :<- [:doc/candidates]
 (fn [candidates _] (vec (operations/selected-candidates (candidates-doc candidates)))))

(rf/reg-sub
 :candidates/selected-ids
 :<- [:doc/candidates]
 (fn [candidates _] (operations/selected-candidates-ids (candidates-doc candidates))))

(rf/reg-sub
 :candidates/in-selected-groups
 ;; the ids of all candidates in the same group as any selected candidate
 :<- [:doc/candidates]
 (fn [candidates _]
   (let [groups (->> (for [[id {g ::demand/group}] candidates
                           :when g] [g id])
                     (group-by first)
                     (S/multi-transform
                      [S/MAP-VALS (S/multi-path
                                   [S/ALL (S/terminal second)]
                                   (S/terminal set))]))]
     (set
      (for [[_ {g ::demand/group s ::candidate/selected}] candidates
            :when (and s g)
            adj (groups g)]
        adj)))))

(rf/reg-sub
 :candidates/selected-user-fields
 ;; a sorted map from the user fields on the selected candidates to the
 ;; type of their values: :none, :bit, :number, :boolean or :string
 :<- [:candidates/selected]
 (fn [selection _]
   (->> selection
        (reduce
         (fn [acc candidate]
           (reduce-kv
            (fn [acc field val]
              (let [new (not (contains? acc field))
                    cur (get acc field :none)
                    nxt (cond
                          (nil? val)               :none
                          (or (= 0 val) (= 1 val)) :bit
                          (number? val)            :number
                          (boolean? val)           :boolean
                          :else                    :string)]
                (cond
                  (or new (= :none cur)) (assoc! acc field nxt)
                  (= cur nxt)            acc
                  (and (= :number nxt)
                       (= :bit cur))     (assoc! acc field :number)
                  :else                  (assoc! acc field :string))))
            acc
            (::candidate/user-fields candidate)))
         (transient {}))
        (persistent!)
        (into (sorted-map)))))

(rf/reg-sub
 :solution/diameter-range
 ;; [smallest largest] pipe diameter in mm in the solution, if there is one
 :<- [:doc/has-solution]
 :<- [:doc/candidates]
 (fn [[has-solution candidates] _]
   (when has-solution
     (reduce
      (fn [[l u] d]
        [(or (and l (min d l)) d)
         (or (and u (max d u)) d)])
      []
      (keep ::solution/diameter-mm (vals candidates))))))

;; View state: the map and the candidates table

(rf/reg-sub :view/state (fn [db _] (::view/view-state db)))

(rf/reg-sub
 :view/map-view
 :<- [:view/state]
 (fn [view-state _] (::view/map-view view-state)))

(rf/reg-sub
 :view/show-pipe-diameters
 :<- [:view/state]
 (fn [view-state _] (::view/show-pipe-diameters view-state)))

(rf/reg-sub
 :map/showing-forbidden
 :<- [:view/state]
 (fn [view-state _] (operations/showing-forbidden? {::view/view-state view-state})))

(rf/reg-sub
 :map/spatial-index
 ;; the counter changes whenever the (mutable) index does
 (fn [db _] (select-keys db [::spatial/spatial-index ::spatial/update-counter])))

(rf/reg-sub
 :map/tile-candidate-ids
 ;; the ids of the candidates in the bounding box [_ bbox]
 :<- [:map/spatial-index]
 (fn [index [_ bbox]] (spatial/find-candidates-ids-in-bbox index bbox)))

(rf/reg-sub
 :table/filters
 :<- [:view/state]
 (fn [view-state _] (operations/get-all-table-filters {::view/view-state view-state})))

(rf/reg-sub
 :table/filter
 ;; the filter on the column [_ key]
 :<- [:table/filters]
 (fn [filters [_ key]] (get filters key)))

(rf/reg-sub
 :table/n-filters
 :<- [:table/filters]
 (fn [filters _] (count-if (vals filters) not-empty)))

(rf/reg-sub
 :table/open-filter
 :<- [:view/state]
 (fn [view-state _] (operations/table-filter-open? {::view/view-state view-state})))

(rf/reg-sub
 :table/filtered-candidates
 :<- [:doc/candidates]
 :<- [:view/state]
 (fn [[candidates view-state] _]
   (vec
    (operations/get-filtered-candidates
     {::document/candidates candidates ::view/view-state view-state}))))

(rf/reg-sub
 :table/filtered-candidate-ids
 :<- [:table/filtered-candidates]
 (fn [candidates _] (set (map ::candidate/id candidates))))

;; Saving and running

(rf/reg-sub
 :server
 (fn [db _]
   (db/server db)))

(rf/reg-sub
 :server/needs-save
 :<- [:server]
 (fn [server _] (:needs-save server)))

(rf/reg-sub
 :server/running-or-queued
 :<- [:server]
 (fn [server _] (contains? db/running-states (:run-state server))))

(rf/reg-sub
 :server/run
 :<- [:server]
 (fn [server _]
   (select-keys server [:run-state :queue-position :run-message :run-timing])))

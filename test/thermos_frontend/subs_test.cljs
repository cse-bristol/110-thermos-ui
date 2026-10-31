;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.subs-test
  (:require [cljs.test :refer [deftest testing is use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.document :as document]
            [thermos-specs.view :as view]
            [thermos-frontend.events]
            [thermos-frontend.subs]))

(use-fixtures :each
  {:before #(set! (.-restore js/goog.global) (rf/make-restore-fn))
   :after  #((.-restore js/goog.global))})

(def candidates
  {"a" {::candidate/id "a" ::candidate/type :building ::candidate/inclusion :optional
        ::candidate/selected true ::demand/group 1
        ::candidate/user-fields {"Height" 10 "Use" "house" "Listed" 1}}
   "b" {::candidate/id "b" ::candidate/type :building ::candidate/inclusion :required
        ::demand/group 1
        ::candidate/user-fields {"Height" 2.5 "Use" nil}}
   "c" {::candidate/id "c" ::candidate/type :path ::candidate/inclusion :forbidden}})

(defn- sub [db query]
  (reset! re-frame.db/app-db db)
  @(rf/subscribe query))

(deftest candidates-subs
  (let [db {::document/candidates candidates}]
    (is (= #{"a"} (sub db [:candidates/selected-ids])))
    (is (= #{"a" "b"} (set (map ::candidate/id (sub db [:candidates/included])))))
    (is (= #{"a" "b"} (sub db [:candidates/in-selected-groups]))
        "b is grouped with a, which is selected")
    (is (= {"Height" :number "Listed" :bit "Use" :string}
           (sub db [:candidates/selected-user-fields])))))

(deftest table-filters
  (let [after (fn [& events]
                (reset! re-frame.db/app-db {::document/candidates candidates})
                (run! rf/dispatch-sync events)
                @re-frame.db/app-db)]
    (testing "with no filters, the table shows the constrained candidates"
      (is (= #{"a" "b"} (sub {::document/candidates candidates} [:table/filtered-candidate-ids]))))

    (testing "filtering on a column"
      (let [db (after [:table/add-filter-value ::candidate/inclusion :required])]
        (is (= #{"b"} (sub db [:table/filtered-candidate-ids])))
        (is (= 1 (sub db [:table/n-filters])))
        (is (= #{:required} (sub db [:table/filter ::candidate/inclusion])))))

    (testing "clearing filters"
      (let [db (after [:table/add-filter-value ::candidate/inclusion :required]
                      [:table/clear-filters])]
        (is (= 0 (sub db [:table/n-filters])))))

    (testing "opening and closing a column's filter"
      (is (= ::candidate/inclusion (sub (after [:table/open-filter ::candidate/inclusion])
                                        [:table/open-filter])))
      (is (nil? (sub (after [:table/open-filter ::candidate/inclusion] [:table/close-filter])
                     [:table/open-filter]))))))

(deftest selection-events
  (reset! re-frame.db/app-db {::document/candidates candidates})
  (rf/dispatch-sync [:deselect-ids ["a"]])
  (is (empty? (sub @re-frame.db/app-db [:candidates/selected-ids]))))

(deftest view-subs
  (is (true? (sub {} [:map/showing-forbidden])))
  (is (false? (sub {::view/view-state {::view/hide-forbidden true}} [:map/showing-forbidden])))
  (is (= ::view/solution (sub {::view/view-state {::view/map-view ::view/solution}} [:view/map-view]))))

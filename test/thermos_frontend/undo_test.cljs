;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.undo-test
  (:require [cljs.test :refer [deftest testing is use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.document :as document]
            [thermos-specs.view :as view]
            [thermos-frontend.db :as db]
            [thermos-frontend.events]
            [thermos-frontend.subs]
            [thermos-frontend.operations :as operations]
            [thermos-frontend.undo :as undo]))

(use-fixtures :each
  {:before #(set! (.-restore js/goog.global) (rf/make-restore-fn))
   :after  #((.-restore js/goog.global))})

(def server
  {:project-id 1 :map-id 2 :network-id 3
   :needs-save false :needs-load false
   :queue-position 0 :run-state nil :revision 0})

(def candidates
  {"a" {::candidate/id "a" ::candidate/type :building ::candidate/inclusion :optional}
   "b" {::candidate/id "b" ::candidate/type :building ::candidate/inclusion :required}})

;; without candidates, which have no geometry for the spatial index that
;; undo and redo update
(def a-db
  {::document/npv-term 10
   ::view/view-state {::view/map-colouring :constraint}
   ::db/server server})

(defn- dispatch! [& events]
  (run! rf/dispatch-sync events)
  @re-frame.db/app-db)

(defn- npv [db] (::document/npv-term db))

(defn- depth [db] [(count (:undo (undo/history db))) (count (:redo (undo/history db)))])

(defn- set-npv [db n t] (undo/record db (assoc db ::document/npv-term n) [:doc/set [::document/npv-term] n] t))

(deftest undo-and-redo
  (reset! re-frame.db/app-db a-db)
  (is (not @(rf/subscribe [:undo/can-undo])) "nothing to undo to start with")
  (dispatch! [:doc/set [::document/npv-term] 20]
             [:doc/set [::document/discount-rate] 0.1])
  (is (= [2 0] (depth @re-frame.db/app-db)) "an entry for each edit")
  (is @(rf/subscribe [:undo/can-undo]))
  (is (not @(rf/subscribe [:undo/can-redo])))

  (swap! re-frame.db/app-db db/saved {})
  (testing "undoing"
    (let [db (dispatch! [:doc/undo])]
      (is (= 20 (npv db)))
      (is (nil? (::document/discount-rate db)))
      (is (= [1 1] (depth db))))
    (let [db (dispatch! [:doc/undo])]
      (is (= 10 (npv db)))
      (is (= [0 2] (depth db)))
      (is (db/needs-save? db) "the document is not what was saved"))
    (is (= 10 (npv (dispatch! [:doc/undo]))) "and no further"))

  (testing "redoing"
    (let [db (dispatch! [:doc/redo])]
      (is (= 20 (npv db)))
      (is (= [1 1] (depth db)))))

  (testing "a new edit forgets what could be redone"
    (let [db (dispatch! [:doc/set [::document/npv-term] 40])]
      (is (= [2 0] (depth db)))
      (is (not @(rf/subscribe [:undo/can-redo])))
      (is (= 20 (npv (dispatch! [:doc/undo])))))))

(deftest what-is-kept
  (reset! re-frame.db/app-db a-db)
  (dispatch! [:doc/set [::document/npv-term] 20]
             [:doc/set [::view/view-state ::view/map-colouring] :solution]
             [:doc/set [::db/server :run-message] "hello"])
  (let [db (dispatch! [:doc/undo])]
    (is (= 10 (npv db)))
    (is (= :solution (get-in db [::view/view-state ::view/map-colouring]))
        "the view state stays as it is")
    (is (= "hello" (:run-message (db/server db))) "and so does the server state")))

(deftest what-is-an-edit
  (let [db (assoc a-db ::document/candidates candidates)]
    (is (undo/edit? db (assoc db ::document/npv-term 3)))
    (is (undo/edit? db (operations/set-candidates-inclusion db ["a"] :forbidden)))
    (is (undo/edit? db (update db ::document/candidates dissoc "a")))
    (is (not (undo/edit? db (operations/select-candidates db ["a" "b"] :replace)))
        "selecting is not")
    (is (not (undo/edit? db (operations/move-map db {:north 1 :south 0 :east 1 :west 0})))
        "nor moving the map")
    (is (not (undo/edit? db (assoc db ::document/migration-messages ["x"]))))
    (is (not (undo/edit? db (assoc-in db [::db/server :run-state] :ready))))
    (is (not (undo/edit? db (assoc db :thermos-frontend.spatial/update-counter 3))))
    (is (undo/edit? db (-> (operations/select-candidates db ["a"] :replace)
                           (assoc ::document/npv-term 3)))
        "an edit and a selection together is an edit")

    (testing "by event"
      (reset! re-frame.db/app-db db)
      (dispatch! [:select-ids ["a"]]
                 [:doc/edit operations/move-map {:north 1 :south 0 :east 1 :west 0}])
      (is (= [0 0] (depth @re-frame.db/app-db)))
      (dispatch! [:doc/edit operations/rotate-candidates-inclusion ["a"]])
      (is (= [1 0] (depth @re-frame.db/app-db))))))

(deftest saving-and-loading
  (let [edited (-> a-db (set-npv 20 0) (set-npv 30 5000))]
    (is (= [2 0] (depth edited)))
    (is (= [2 0] (depth (undo/record edited (db/saved edited {}) [:server/saved] 9000)))
        "saving is not an edit, and keeps the history")

    (testing "loading forgets the history"
      (reset! re-frame.db/app-db edited)
      (let [db (dispatch! [:server/loaded {:document {::document/npv-term 99}}])]
        (is (= 99 (npv db)))
        (is (= [0 0] (depth db)))
        (is (not (db/needs-save? db)))))))

(deftest coalescing
  (let [db (-> a-db (set-npv 11 0) (set-npv 12 500) (set-npv 13 1200))]
    (is (= [1 0] (depth db)) "typing into the same input")
    (is (= 10 (npv (undo/undo db))) "undoes to before the typing"))
  (is (= [2 0] (depth (-> a-db (set-npv 11 0) (set-npv 12 1500))))
      "but not after a pause")
  (is (= [2 0] (depth (-> a-db
                          (set-npv 11 0)
                          (as-> db (undo/record db (assoc db ::document/discount-rate 1)
                                                [:doc/set [::document/discount-rate] 1] 100)))))
      "nor a different input")
  (is (= [2 0] (depth (-> a-db (set-npv 11 0) (set-npv 12 100) (undo/undo) (undo/redo)
                          (set-npv 13 200))))
      "nor after undoing or redoing"))

(deftest depth-is-capped
  (let [db (reduce #(set-npv %1 %2 (* %2 2000)) a-db (range 100 (+ 110 undo/max-depth)))]
    (is (= [undo/max-depth 0] (depth db)))
    (is (= 109 (npv (nth (iterate undo/undo db) undo/max-depth)))
        "the oldest edits are forgotten")))

(deftest while-running
  (reset! re-frame.db/app-db a-db)
  (dispatch! [:doc/set [::document/npv-term] 20]
             [:doc/set [::db/server :run-state] :running])
  (is (not @(rf/subscribe [:undo/can-undo])))
  (is (= 20 (npv (dispatch! [:doc/undo]))) "the document being optimised stays as it is"))

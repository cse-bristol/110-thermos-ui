;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.events-test
  (:require [cljs.test :refer [deftest testing is use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.document :as document]
            [thermos-frontend.db :as db]
            [thermos-frontend.events :as events]
            [thermos-frontend.fx :as fx]
            [thermos-frontend.operations :as operations]
            [thermos-frontend.spatial :as spatial]))

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

(def a-db
  {::document/candidates candidates
   ::document/pipe-costs {:rows {100 {:pipe 1} 200 {:pipe 2}}}
   ::db/server server})

(defn- after [& events]
  (reset! re-frame.db/app-db a-db)
  (run! rf/dispatch-sync events)
  @re-frame.db/app-db)

(deftest document-events
  (is (= 4 (::document/npv-term (after [:doc/edit assoc ::document/npv-term 4]))))
  (is (= 5 (::document/npv-term (after [:doc/set [::document/npv-term] 5]))))
  (is (= 3 (get-in (after [:doc/update [::document/pipe-costs :rows 100 :pipe] + 2])
                   [::document/pipe-costs :rows 100 :pipe])))
  (testing "geometry events update the spatial index"
    (is (some? (::spatial/spatial-index
                (after [:doc/edit-geometry dissoc ::document/candidates]))))))

(deftest selection
  (is (= #{"a"} (set (operations/selected-candidates-ids (after [:select-ids ["a"]])))))
  (is (= #{"a" "b"} (set (operations/selected-candidates-ids (after [:select-all]))))))

(deftest pipe-costs
  (is (= #{150 200} (set (keys (get-in (after [:pipe/change-diameter 100 150])
                                       [::document/pipe-costs :rows])))))
  (is (= {:pipe 1 :capacity-kw 9}
         (get-in (after [:pipe/change-capacity 100 9]) [::document/pipe-costs :rows 100])))
  (is (= {:pipe 1}
         (get-in (after [:pipe/change-capacity 100 9] [:pipe/change-capacity 100 nil])
                 [::document/pipe-costs :rows 100]))))

(deftest unsaved-changes
  (let [changed (assoc a-db ::document/npv-term 99)]
    (is (db/unsaved-change? a-db changed) "a change to the document")
    (is (not (db/unsaved-change? a-db (assoc a-db ::spatial/update-counter 9)))
        "a change to something transient")
    (is (not (db/unsaved-change? a-db (assoc-in changed [::db/server :needs-save] true)))
        "already unsaved")
    (is (db/unsaved-change? a-db (assoc-in changed [::db/server :run-state] :ready))
        "a change to the run state as well")
    (is (not (db/unsaved-change? a-db (db/saved changed {})))
        "saving or loading")))

(deftest needs-save
  (testing "changing the document"
    (is (db/needs-save? (after [:doc/set [::document/npv-term] 7]))))
  (testing "changing something transient"
    (is (not (db/needs-save? (after [:doc/set [::spatial/update-counter] 7])))))
  (testing "loading"
    ;; without candidates, which have no geometry for the spatial index here
    (reset! re-frame.db/app-db (dissoc a-db ::document/candidates))
    (rf/dispatch-sync [:doc/set [::document/npv-term] 7])
    (is (db/needs-save? @re-frame.db/app-db))
    (rf/dispatch-sync [:server/loaded {:document {::document/npv-term 30}}])
    (is (not (db/needs-save? @re-frame.db/app-db)))))

(deftest saving
  (testing "the request"
    (let [{{:keys [run content on-response]} ::fx/save}
          (events/save {:db (assoc a-db ::spatial/update-counter 1)} [:server/save "name" :network nil])]
      (is (= :network run))
      (is (= [:server/saved :network nil] on-response))
      (is (not (re-find #"update-counter|server" content)) "transient keys are not saved")))

  (testing "a successful save"
    (let [callback identity
          {:keys [db fx]} (events/saved {:db (assoc-in a-db [::db/server :needs-save] true)}
                                        [:server/saved :network callback
                                         {:status 201
                                          :location "/project/1/map/2/net/7"
                                          :run-state :ready
                                          :queue-position 4}])]
      (is (= (assoc server :network-id 7 :run-state :ready :queue-position 4 :revision 1)
             (db/server db)))
      (is (= #{[::fx/replace-url "/project/1/map/2/net/7"]
               [:dispatch [:server/poll]]
               [::fx/call [callback 201]]}
             (set (remove nil? fx))))))

  (testing "a failed save changes nothing but tells the callback"
    (let [{:keys [db fx]} (events/saved {:db a-db} [:server/saved nil identity {:status 403}])]
      (is (= a-db db))
      (is (= [[::fx/call [identity 403]]] (remove nil? fx))))))

(deftest loading
  (is (nil? (events/load {:db (assoc-in a-db [::db/server :network-id] :new)} [:server/load])))
  (is (= {::fx/load {:on-response [:server/loaded]}} (events/load {:db a-db} [:server/load])))

  (let [loaded (events/loaded (assoc-in a-db [::db/server :needs-save] true)
                              [:server/loaded {:document {::document/candidates {"c" {::candidate/id "c"}}
                                                          ::document/npv-term 30}
                                               :run-state :completed}])]
    (is (= 30 (::document/npv-term loaded)))
    (is (= #{"a" "b" "c"} (set (keys (::document/candidates loaded)))))
    (is (= (assoc server :run-state :completed :queue-position nil :revision 1) (db/server loaded))
        "keeps the server state, which the loaded document doesn't have"))

  (testing "loading a solution when the run state is already up to date"
    (let [before (update a-db ::db/server assoc :run-state :completed :queue-position nil)
          after  (events/loaded before [:server/loaded {:document {::document/npv-term 30}
                                                         :run-state :completed}])]
      (is (not (db/unsaved-change? before after))))))

(deftest polling
  (letfn [(status [last-state new-state]
            (events/run-status {:db (assoc-in a-db [::db/server :run-state] last-state)}
                               [:server/run-status {:state new-state :message "log"}]))]
    (is (= [[::fx/poll [:server/poll]]] (:fx (status nil :running))))
    (is (= "log" (:run-message (db/server (:db (status nil :running))))))
    (is (= [[::fx/poll nil] [:dispatch [:server/load]]] (:fx (status :running :completed)))
        "loads the solution when a run finishes")
    (is (= [[::fx/poll nil]] (:fx (status nil :completed)))
        "but not when the page opens on a finished run")))

;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.events
  "re-frame events for the editor. app-db is the document (see
  thermos-frontend.db), so most of these are the functions in
  thermos-specs.document and thermos-frontend.operations."
  (:require [re-frame.core :as rf]
            [com.rpl.specter :as sr]
            [thermos-util :refer [assoc-id]]
            [thermos-specs.document :as document]
            [thermos-frontend.db :as db]
            [thermos-frontend.fx :as fx]
            [thermos-frontend.transit :as transit]
            [thermos-frontend.operations :as operations]
            [thermos-frontend.spatial :as spatial]))

(def mark-unsaved-changes
  "Notes in the ::db/server state when an event has changed the document in
  a way that needs saving (see db/unsaved-change?). A global interceptor,
  so it comes first and its :after sees the final db."
  (rf/->interceptor
   :id ::mark-unsaved-changes
   :after
   (fn [context]
     (let [old-db (rf/get-coeffect context :db)
           new-db (rf/get-effect context :db)]
       (cond-> context
         (and new-db (db/unsaved-change? old-db new-db))
         (rf/assoc-effect :db (assoc-in new-db [::db/server :needs-save] true)))))))

(rf/reg-global-interceptor mark-unsaved-changes)

(rf/reg-event-db
 :editor/initialise
 (fn [_ _] (db/initial-db)))

(def spatial-index
  "For events which change candidates' geometry, or which candidates there
  are: brings the spatial index up to date afterwards"
  (rf/enrich (fn [db _] (spatial/update-index db))))

;; Changing the document generally

(rf/reg-event-db
 :doc/edit
 (fn [db [_ f & args]]
   (apply f db args)))

(rf/reg-event-db
 :doc/edit-geometry
 [spatial-index]
 (fn [db [_ f & args]]
   (apply f db args)))

(rf/reg-event-db
 :doc/set
 (fn [db [_ path value]]
   (assoc-in db path value)))

(rf/reg-event-db
 :doc/update
 (fn [db [_ path f & args]]
   (apply update-in db path f args)))

;; Selection and groups

(rf/reg-event-db
 :select-all
 (fn [db _]
   (operations/select-all-candidates db)))

(rf/reg-event-db
 :select-ids
 (fn [db [_ ids method]]
   (operations/select-candidates db ids (or method :replace))))

(rf/reg-event-db
 :deselect-ids
 (fn [db [_ ids]]
   (operations/deselect-candidates db ids)))

(rf/reg-event-db
 :group-selection
 (fn [db _]
   (document/group-candidates db (operations/selected-candidates-ids db))))

(rf/reg-event-db
 :ungroup-selection
 (fn [db _]
   (document/ungroup-candidates db (operations/selected-candidates-ids db))))

(rf/reg-event-db
 :group-select-members
 (fn [db _]
   (document/select-group-members db)))

;; The candidates table's column filters

(rf/reg-event-db
 :table/open-filter
 (fn [db [_ key]] (operations/open-table-filter db key)))

(rf/reg-event-db
 :table/close-filter
 ;; this happens on any click in the editor, so it mustn't change
 ;; anything unless a filter is open
 (fn [db _]
   (cond-> db
     (operations/table-filter-open? db) (operations/close-table-filter))))

(rf/reg-event-db
 :table/clear-filters
 (fn [db _] (operations/clear-filters db)))

(rf/reg-event-db
 :table/set-filter
 (fn [db [_ key value]] (operations/set-table-filter-value db key value)))

(rf/reg-event-db
 :table/remove-filter
 (fn [db [_ key]] (operations/remove-all-table-filter-values db key)))

(rf/reg-event-db
 :table/add-filter-value
 (fn [db [_ key value]] (operations/add-table-filter-value db key value)))

(rf/reg-event-db
 :table/add-filter-values
 (fn [db [_ key values]] (operations/add-table-filter-values db key values)))

(rf/reg-event-db
 :table/remove-filter-value
 (fn [db [_ key value]] (operations/remove-table-filter-value db key value)))

(rf/reg-event-db
 :doc/dismiss-migration-messages
 (fn [db _] (dissoc db ::document/migration-messages)))

;; Pipe costs

(rf/reg-event-db
 :pipe/change-diameter
 (fn [db [_ from to]]
   (sr/setval [::document/pipe-costs :rows sr/MAP-KEYS (sr/pred= from)] to db)))

(rf/reg-event-db
 :pipe/rename-civils
 (fn [db [_ cid name]]
   (assoc-in db [::document/pipe-costs :civils cid] name)))

(rf/reg-event-db
 :pipe/change-cost
 (fn [db [_ dia cost]]
   (assoc-in db [::document/pipe-costs :rows dia :pipe] cost)))

(rf/reg-event-db
 :pipe/change-civil-cost
 (fn [db [_ dia cid cost]]
   (assoc-in db [::document/pipe-costs :rows dia cid] cost)))

(rf/reg-event-db
 :pipe/add-diameter
 (fn [db [_ dia]]
   (assoc-in db [::document/pipe-costs :rows dia] {})))

(rf/reg-event-db
 :pipe/add-civils
 (fn [db [_ n]]
   (update-in db [::document/pipe-costs :civils] assoc-id n)))

(rf/reg-event-db
 :pipe/remove-diameter
 (fn [db [_ dia]]
   (update-in db [::document/pipe-costs :rows] dissoc dia)))

(rf/reg-event-db
 :pipe/remove-civils
 (fn [db [_ cid]]
   (document/remove-civils db cid)))

(defn- assoc-or-dissoc-row-number
  "Set `k` in the pipe costs row for `dia` to `x`, or remove it if `x` is
  not a number"
  [db dia k x]
  (if (number? x)
    (assoc-in db [::document/pipe-costs :rows dia k] x)
    (update-in db [::document/pipe-costs :rows dia] dissoc k)))

(rf/reg-event-db
 :pipe/change-capacity
 (fn [db [_ dia capacity-kw]]
   (assoc-or-dissoc-row-number db dia :capacity-kw capacity-kw)))

(rf/reg-event-db
 :pipe/change-losses
 (fn [db [_ dia losses-kwh]]
   (assoc-or-dissoc-row-number db dia :losses-kwh losses-kwh)))

(rf/reg-event-db
 :pipe/change-flow-temperature
 (fn [db [_ t]]
   (assoc db ::document/flow-temperature t)))

(rf/reg-event-db
 :pipe/change-return-temperature
 (fn [db [_ t]]
   (assoc db ::document/return-temperature t)))

(rf/reg-event-db
 :pipe/change-ground-temperature
 (fn [db [_ t]]
   (assoc db ::document/ground-temperature t)))

(rf/reg-event-db
 :pipe/set-default-civils
 (fn [db [_ d]]
   (assoc-in db [::document/pipe-costs :default-civils] d)))

;; Saving, loading and running. The handlers are functions of their own
;; so they can be tested without the effects happening.

(defn save [{db :db} [_ title run callback]]
  {::fx/save {:title title
              :run run
              :content (transit/write (document/keep-interesting db))
              :on-response [:server/saved run callback]}})

(defn saved [{db :db} [_ run callback {:keys [status location run-state queue-position]}]]
  (let [ok (<= 200 status 299)]
    {:db (cond-> db
           ok (db/saved (merge (db/parse-network-url location)
                               {:run-state run-state
                                :queue-position queue-position})))
     :fx [(when ok [::fx/replace-url location])
          (when (and ok run) [:dispatch [:server/poll]])
          (when callback [::fx/call [callback status]])]}))

(defn load [{db :db} _]
  (when (not= :new (:network-id (db/server db)))
    {::fx/load {:on-response [:server/loaded]}}))

(defn loaded [db [_ {:keys [document run-state queue-position]}]]
  ;; remove-solution is needed in case there is an element that got
  ;; solution data last run but is not in the problem we sent to the server
  (-> (document/remove-solution db)
      (operations/load-document document)
      ;; the loaded document has no ::db/server, so carry it over
      (assoc ::db/server (db/server db))
      (db/saved {:run-state run-state
                 :queue-position queue-position})))

(defn poll [{db :db} _]
  ;; a network which has never been saved has no run status to ask for
  (when (not= :new (:network-id (db/server db)))
    {::fx/run-status {:on-response [:server/run-status]}}))

(defn run-status [{db :db} [_ {:keys [state queue-position message timing]}]]
  (let [last-run-state (:run-state (db/server db))]
    {:db (update db ::db/server assoc
                 :run-state state
                 :queue-position queue-position
                 :run-message message
                 :run-timing timing)
     :fx (cond
           (contains? db/running-states state)
           [[::fx/poll [:server/poll]]
            ;; see thermos-frontend.push-offer
            (when (#{:ready :running} state)
              [:dispatch [:push-offer/consider timing]])]

           (and (= :completed state) (#{:ready :running} last-run-state))
           [[::fx/poll nil]
            [:dispatch [:server/load]]]

           :else
           [[::fx/poll nil]])}))

(rf/reg-event-fx :server/save save)
(rf/reg-event-fx :server/saved saved)
(rf/reg-event-fx :server/load load)
(rf/reg-event-db :server/loaded [spatial-index] loaded)
(rf/reg-event-fx :server/poll poll)
(rf/reg-event-fx :server/run-status run-status)

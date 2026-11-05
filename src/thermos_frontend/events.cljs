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
            [thermos-frontend.spatial :as spatial]
            [thermos-frontend.undo :as undo]))

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

(def record-undo
  "Keeps the undo history (see thermos-frontend.undo) up to date with each
  event's change to the document, except for undoing and redoing
  themselves. A global interceptor, so its :after sees the final db."
  (rf/->interceptor
   :id ::record-undo
   :after
   (fn [context]
     (let [old-db (rf/get-coeffect context :db)
           new-db (rf/get-effect context :db)
           event  (rf/get-coeffect context :event)]
       (cond-> context
         (and new-db (not (#{:doc/undo :doc/redo} (first event))))
         (rf/assoc-effect :db (undo/record old-db new-db event (js/Date.now))))))))

(rf/reg-global-interceptor record-undo)

(rf/reg-event-db
 :editor/initialise
 (fn [_ _] (db/initial-db)))

(def spatial-index
  "For events which change candidates' geometry, or which candidates there
  are: brings the spatial index up to date afterwards"
  (rf/enrich (fn [db _] (spatial/update-index db))))

;; Undo and redo. They change the document like any other edit, so
;; mark-unsaved-changes notes that it needs saving.

(rf/reg-event-db :doc/undo [spatial-index] (fn [db _] (undo/undo db)))

(rf/reg-event-db :doc/redo [spatial-index] (fn [db _] (undo/redo db)))

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
  (when (number? (:network-id (db/server db)))
    {::fx/load {:on-response [:server/loaded]}}))

(defn loaded [db [_ {:keys [document run-state queue-position]}]]
  ;; remove-solution is needed in case there is an element that got
  ;; solution data last run but is not in the problem we sent to the server
  (-> (document/remove-solution db)
      (operations/load-document document)
      ;; the loaded document has no ::db/server, so carry it over, and
      ;; any comparison
      (merge (select-keys db [::db/server ::db/compare]))
      (db/saved {:run-state run-state
                 :queue-position queue-position})))

(defn poll [{db :db} _]
  ;; a network which has never been saved, or a parameter set, has no
  ;; run status to ask for
  (when (number? (:network-id (db/server db)))
    {::fx/run-status {:on-response [:server/run-status]}}))

(defn run-status [{db :db} [_ {:keys [state queue-position message timing objectives]}]]
  (let [last-run-state (:run-state (db/server db))]
    {:db (update db ::db/server assoc
                 :run-state state
                 :queue-position queue-position
                 :run-message message
                 :run-timing timing
                 :run-objectives objectives)
     :fx (cond
           (contains? db/running-states state)
           [[::fx/poll [:server/poll]]
            ;; see thermos-frontend.push-offer
            (when (#{:ready :running} state)
              [:dispatch [:push-offer/consider timing]])]

           ;; a stopped run completes too, with its best solution so far
           (and (= :completed state) (contains? db/running-states last-run-state))
           [[::fx/poll nil]
            [:dispatch [:server/load]]]

           :else
           [[::fx/poll nil]])}))

(defn stop [{db :db} _]
  (when (db/running-or-queued? db)
    {:db (update db ::db/server assoc :run-state :cancel)
     ::fx/stop {:url (db/network-url (db/server db) "/stop")}}))

(defn continue-editing [{db :db} _]
  ;; stop polling now, so the run finishing can't load its result into
  ;; the document while the names are fetched
  {:fx [[::fx/poll nil]
        [::fx/network-names {:url (db/network-url (assoc (db/server db) :network-id :names))
                             :on-response [:server/continue-editing-as]}]]})

(defn continue-editing-as
  "Leave the run to finish by itself, and carry on editing the document as
  a new network, named after it but not the same as any on the map"
  [{db :db} [_ names]]
  (let [server (assoc (db/server db)
                      :network-id :new
                      :needs-save true
                      :needs-load false
                      :run-state nil
                      :queue-position 0
                      :run-message nil
                      :run-timing nil
                      :run-objectives nil)]
    {:db (assoc db ::db/server server)
     :fx [[::fx/poll nil]
          [::fx/replace-url (db/network-url server)]
          [:thermos-frontend.main-nav/rename-as-copy names]]}))

(rf/reg-event-fx :server/stop stop)
(rf/reg-event-fx :server/continue-editing continue-editing)
(rf/reg-event-fx :server/continue-editing-as continue-editing-as)

(rf/reg-event-fx :server/save save)
(rf/reg-event-fx :server/saved saved)
(rf/reg-event-fx :server/load load)
(rf/reg-event-db :server/loaded [spatial-index] loaded)
(rf/reg-event-fx :server/poll poll)
(rf/reg-event-fx :server/run-status run-status)

;; Comparing with another network (see db/comparison). None of these
;; change the document, so they aren't edits, and don't need saving.

(defn compare-fetch-networks
  "Fetch the networks on the map, to choose one to compare with"
  [{db :db} _]
  {:db (update db ::db/compare assoc :networks nil)
   ::fx/get-transit {:url (db/network-url (assoc (db/server db) :network-id :history))
                     :on-response [:compare/got-networks]}})

(defn compare-got-networks [db [_ networks]]
  (assoc-in db [::db/compare :networks] (or networks :failed)))

(defn compare-with
  "Fetch the document of `network` (from the list of networks) to compare
  with, carrying on with any comparison until it comes"
  [{db :db} [_ {:keys [id] :as network}]]
  {:db (update db ::db/compare #(-> % (assoc :loading network) (dissoc :failed)))
   ::fx/get-transit {:url (db/network-url (assoc (db/server db) :network-id id))
                     :on-response [:compare/loaded id]}})

(defn compare-loaded
  "Start comparing with the network whose `document` has come, unless the
  user has since chosen another, or stopped comparing"
  [db [_ id document]]
  (let [{:keys [loading]} (db/comparison db)]
    (cond
      (not= id (:id loading))
      db

      document
      (update db ::db/compare
              #(-> %
                   (dissoc :loading)
                   (assoc :network loading
                          :document (spatial/update-index document)
                          :picked #{})))

      :else
      (update db ::db/compare #(-> % (dissoc :loading) (assoc :failed loading))))))

(defn compare-pick
  "Note the candidates in the other network under `shape`, which the user
  has clicked on, so it can show those which aren't in this one. Like
  the selection, `method` :replace replaces them, and otherwise adds to
  them."
  [db [_ shape method]]
  (if (db/comparing? db)
    (let [ids (set (spatial/find-intersecting-candidates-ids
                    (:document (db/comparison db)) shape))]
      (update db ::db/compare update :picked
              #(if (= :replace method) ids (into (or % #{}) ids))))
    db))

(rf/reg-event-fx :compare/fetch-networks compare-fetch-networks)
(rf/reg-event-db :compare/got-networks compare-got-networks)
(rf/reg-event-fx :compare/with compare-with)
(rf/reg-event-db :compare/loaded compare-loaded)
(rf/reg-event-db :compare/pick compare-pick)
(rf/reg-event-db :compare/stop (fn [db _] (dissoc db ::db/compare)))
(rf/reg-event-db :compare/dismiss-failure
                 (fn [db _] (update db ::db/compare dissoc :failed)))

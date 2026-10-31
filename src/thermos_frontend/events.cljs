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
            [thermos-frontend.operations :as operations]
            [thermos-frontend.spatial :as spatial]))

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
              :content (pr-str (document/keep-interesting db))
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

(defn run-status [{db :db} [_ {:keys [state queue-position message timing]}]]
  (let [last-run-state (:run-state (db/server db))]
    {:db (update db ::db/server assoc
                 :run-state state
                 :queue-position queue-position
                 :run-message message
                 :run-timing timing)
     :fx (cond
           (contains? db/running-states state)
           [[::fx/poll [:server/poll]]]

           (and (= :completed state) (#{:ready :running} last-run-state))
           [[::fx/poll nil]
            [:dispatch [:server/load]]]

           :else
           [[::fx/poll nil]])}))

(rf/reg-event-fx :server/save save)
(rf/reg-event-fx :server/saved saved)
(rf/reg-event-fx :server/load load)
(rf/reg-event-db :server/loaded [spatial-index] loaded)
(rf/reg-event-fx :server/poll (fn [_ _] {::fx/run-status {:on-response [:server/run-status]}}))
(rf/reg-event-fx :server/run-status run-status)

;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.notifications
  "Telling people when model runs and map imports they started have
  finished, by email if they have asked for that in their settings, and
  by push notification to any browsers they have turned that on in."
  (:require [honeysql.helpers :as h]
            [honeysql-postgres.helpers :as p]
            [honeysql.core :as sql]
            [jdbc.core :as jdbc]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [mount.core :refer [defstate]]
            [thermos-backend.config :refer [config]]
            [thermos-backend.db :as db]
            [thermos-backend.email :as email]
            [thermos-backend.queue :as queue]
            [thermos-backend.web-push :as web-push]))

(def ^:private min-seconds
  "Jobs finishing sooner than this after they were queued aren't worth
  telling anyone about, as they will have watched them finish."
  60)

(defn- load-keys [conn]
  (-> (h/select :public-key :private-key)
      (h/from :vapid-keys)
      (db/fetch-one! conn)))

(defn- ensure-keys!
  "The VAPID keys we sign pushes with, made if there are none. Browsers'
  subscriptions are tied to the key they were made with, so any we have
  are useless once the keys change."
  []
  (db/with-connection [conn]
    (or (load-keys conn)
        (jdbc/atomic conn
          (let [inserted (-> (h/insert-into :vapid-keys)
                             (h/values [(web-push/generate-keys)])
                             (p/upsert (-> (p/on-conflict :id) (p/do-nothing)))
                             (db/execute! conn))]
            (when (and inserted (pos? inserted))
              (log/info "Made new VAPID keys, forgetting old push subscriptions")
              (db/execute! (h/delete-from :push-subscriptions) conn))
            (load-keys conn))))))

(defstate vapid-keys :start (ensure-keys!))

(defn public-key [] (:public-key vapid-keys))

(defn- subject
  "A URL push services can contact us at if we cause trouble"
  []
  (let [address (some #(when-not (string/blank? %) %)
                      [(config :contact-email) (config :smtp-from-address)])]
    (if address
      (str "mailto:" address)
      (config :base-url))))

(defn subscribe!
  "Remember a browser's push subscription for `user-id`. A browser only
  has one subscription, so if someone else had it, now they don't."
  [user-id {:keys [endpoint p256dh auth]}]
  {:pre [(string? user-id) (string? endpoint) (string? p256dh) (string? auth)
         (string/starts-with? endpoint "https://")]}
  (-> (h/insert-into :push-subscriptions)
      (h/values [{:endpoint endpoint :user-id user-id :p256dh p256dh :auth auth}])
      (p/upsert (-> (p/on-conflict :endpoint)
                    (p/do-update-set :user-id :p256dh :auth)))
      (db/execute!)))

(defn unsubscribe! [user-id endpoint]
  (-> (h/delete-from :push-subscriptions)
      (h/where [:and [:= :endpoint endpoint] [:= :user-id user-id]])
      (db/execute!)))

(defn- subscriptions [user-id]
  (-> (h/select :endpoint :p256dh :auth)
      (h/from :push-subscriptions)
      (h/where [:= :user-id user-id])
      (db/fetch!)))

(defn push!
  "Send `message` to all of `user-id`'s browsers, forgetting any that the
  push service says have gone. Returns how many it went to."
  [user-id message]
  (let [keys vapid-keys
        subject (subject)]
    (count
     (filter
      (fn [subscription]
        (try
          (let [status (web-push/send! subscription message :keys keys :subject subject)]
            (cond
              (<= 200 status 299) true
              (#{404 410} status)
              (do (log/info "Push subscription gone" (:endpoint subscription))
                  (-> (h/delete-from :push-subscriptions)
                      (h/where [:= :endpoint (:endpoint subscription)])
                      (db/execute!))
                  false)
              :else
              (do (log/warn "Push to" (:endpoint subscription) "failed with" status)
                  false)))
          (catch Exception e
            (log/error e "Push to" (:endpoint subscription) "failed")
            false)))
      (subscriptions user-id)))))

(defn notify!
  "Tell `user-id` something, by whichever means they have chosen.
  `tag` identifies what it's about, so a later message about the same
  thing replaces the earlier one."
  [user-id {:keys [title body url tag]}]
  (when user-id
    (let [user (-> (h/select :id :job-emails)
                   (h/from :users)
                   (h/where [:= :id user-id])
                   (db/fetch-one!))]
      (when user
        (when (:job-emails user)
          (email/send-job-message user-id title body url))
        (push! user-id {:title title :body body :url url :tag tag})))))

(defn- seconds-since-queued [job-id]
  (-> (h/select [(sql/raw "extract(epoch from (now() at time zone 'utc') - queued)") :seconds])
      (h/from :jobs)
      (h/where [:= :id job-id])
      (db/fetch-one!)
      (:seconds)))

(defn- worth-telling? [{:keys [state job-id]}]
  (and (not= :cancelled state)
       (>= (or (seconds-since-queued job-id) 0) min-seconds)))

(defn- problem-outcome
  "What happened to a model run, from the summary of the network it saved"
  [state {:keys [solution-state objective-value]}]
  (cond
    (not= :completed state)      "failed"
    (number? objective-value)    "finished"
    :else
    (case solution-state
      "infeasible"    "finished, but there is no feasible solution"
      "time-limit"    "ran out of time without finding a solution"
      "empty-problem" "finished, but there was nothing to optimise"
      "failed")))

(defn- problem-finished [{{network-id :id} :args state :state :as job}]
  (when (worth-telling? job)
    (when-let [{:keys [name user-id map-id map-name project-id meta]}
               (-> (h/select :networks.name :networks.user-id :networks.meta
                             :networks.map-id [:maps.name :map-name] :maps.project-id)
                   (h/from :networks)
                   (h/join :maps [:= :maps.id :networks.map-id])
                   (h/where [:= :networks.id network-id])
                   (db/fetch-one!))]
      (let [outcome (problem-outcome state meta)]
        (notify! user-id
                 {:title (str "Model run " outcome)
                  :body  (str "The model run for " (or name "your network")
                              " on map " map-name " has " outcome ".")
                  :url   (format "%s/project/%d/map/%d/net/%d"
                                 (config :base-url) project-id map-id network-id)
                  :tag   (str "network-" network-id)})))))

(defn- import-finished [{{:keys [map-id user-id]} :args state :state :as job}]
  (when (and user-id (worth-telling? job))
    (when-let [{:keys [name project-id]}
               (-> (h/select :name :project-id)
                   (h/from :maps)
                   (h/where [:= :id map-id])
                   (db/fetch-one!))]
      (let [outcome (if (= :completed state) "finished" "failed")]
        (notify! user-id
                 {:title (str "Map import " outcome)
                  :body  (str "The import of map " name " has " outcome ".")
                  :url   (format "%s/project/%d" (config :base-url) project-id)
                  :tag   (str "map-" map-id)})))))

(defstate listeners
  :start
  (do (queue/on-finish :problems ::notify problem-finished)
      (queue/on-finish :imports ::notify import-finished)))

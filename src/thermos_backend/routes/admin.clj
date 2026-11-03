;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.routes.admin
  (:require [thermos-backend.pages.admin :as pages]
            [thermos-backend.db.admin :as admin-db]
            [thermos-backend.db.users :as users]
            [thermos-backend.db.projects :as projects]
            [thermos-backend.db.maps :as maps]
            [thermos-backend.project-lidar :as project-lidar]
            [thermos-backend.queue :as queue]
            [thermos-backend.config :refer [config]]
            [thermos-backend.routes.responses :refer :all]
            [thermos-backend.pages.cache-control :as cache-control]
            [ring.util.response :as response]
            [thermos-backend.auth :as auth]
            [clojure.string :as string]))

(defn- me [] (:id auth/*current-user*))

(defn- param
  "Parameter `k`, trimmed, or nil if it's blank"
  [params k]
  (let [v (get params k)]
    (when-not (string/blank? v) (string/trim v))))

(defn- respond
  "A response with some fragments for htmx"
  [& content]
  (-> (apply fragment content) (cache-control/no-store)))

(defn- with-trigger
  "Make htmx fire `events` (a comma separated string) after the response"
  [response events]
  (response/header response "HX-Trigger" events))

(defn- bad-request [message]
  (-> (response/bad-request message)
      (response/content-type "text/plain")
      (cache-control/no-store)))

(defn- whole-page?
  "Whether to send the whole admin page rather than just a tab. htmx asks
  for the whole page when going back, as admin pages aren't kept in its
  history cache."
  [request]
  (or (not (htmx-request? request))
      (= "true" (get-in request [:headers "hx-history-restore-request"]))))

(defn- tab-response [request tab]
  (-> (if (whole-page? request)
        (html (pages/admin-page tab))
        (fragment tab))
      (cache-control/no-store)))

;; Overview

(defn- capacities
  "How many jobs at once each queue's consumer on this machine takes"
  []
  (into {} (for [[q {:keys [capacity]}] @queue/consumers] [(name q) capacity])))

(defn- largest-tables [] (admin-db/table-sizes 12))

(defn- overview [request]
  (auth/verify :sysadmin
    (tab-response
     request
     (pages/overview {:counts        (admin-db/system-counts)
                      :users-by-auth (admin-db/users-by-auth)
                      :queue-counts  (admin-db/queue-counts)
                      :capacities    (capacities)
                      :tables        (largest-tables)
                      :map-bounds    (maps/get-map-bounds-as-geojson)}))))

(defn- queue-action
  "A handler to delete jobs from a queue with `delete!`, which is
  `queue/clean-up` or `queue/erase`"
  [delete! description]
  (fn [{{:keys [queue-name]} :params}]
    (auth/verify :sysadmin
      (let [n (delete! (keyword queue-name))]
        (respond (pages/queue-summary (admin-db/queue-counts) (capacities))
                 (pages/flash (str "Deleted " n " " description " jobs from " queue-name)))))))

(def ^:private clean-queue! (queue-action queue/clean-up "finished"))
(def ^:private purge-queue! (queue-action queue/erase "queued, running and finished"))

(defn- clear-tile-cache! [_]
  (auth/verify :sysadmin
    (admin-db/clear-tile-cache!)
    (respond (pages/table-sizes (largest-tables))
             (pages/flash "Emptied the tile cache"))))

(defn- email-dialog [_]
  (auth/verify :sysadmin
    (respond (pages/email-dialog {:recipients (admin-db/system-message-recipient-count)}))))

(defn- send-email! [{{:keys [subject message]} :params}]
  (auth/verify :sysadmin
    (let [recipients (admin-db/system-message-recipient-count)]
      (if (or (string/blank? subject) (string/blank? message))
        (respond (pages/email-dialog {:subject subject :message message
                                      :recipients recipients
                                      :error "The email needs a subject and a message."}))
        (do (users/send-system-message! subject message)
            (-> (respond (pages/flash (str "Queued the email to " recipients " users")))
                (with-trigger "close-modal")))))))

(defn- get-map-bounds [_]
  (auth/verify :sysadmin
    (json (maps/get-map-bounds-as-geojson))))

;; Users

(defn- users-tab [{{:keys [q]} :params :as request}]
  (auth/verify :sysadmin
    (tab-response request (pages/users-tab (me) q (admin-db/user-summaries q)))))

(defn- users-table [{{:keys [q]} :params}]
  (auth/verify :sysadmin
    (respond (pages/user-table (me) (admin-db/user-summaries q)))))

(defn- user-dialog [{{:keys [id]} :params}]
  (auth/verify :sysadmin
    (if-let [user (and id (admin-db/user-summary id))]
      (respond (pages/user-dialog user (admin-db/user-projects id) (users/num-gis-features id)))
      (response/not-found "No such user"))))

(defn- set-user-auth! [{{:keys [id auth]} :params}]
  (auth/verify :sysadmin
    (let [auth (keyword auth)]
      (cond
        (= id (me))                     (bad-request "You can't change your own authority")
        (not (users/user-auth-types auth)) (bad-request "Unknown authority")
        :else
        (do (users/set-user-auth! id auth)
            (respond (pages/user-row (me) (admin-db/user-summary id))
                     (pages/flash (str id " is now " (name auth)))))))))

(defn- reset-password! [{{:keys [id]} :params}]
  (auth/verify :sysadmin
    (respond
     (if (users/emit-reset-token! id)
       (pages/flash (str "Emailed " id " a link to reset their password"))
       (pages/flash (str "There is no user " id) true)))))

(defn- delete-user! [{{:keys [id]} :params}]
  (auth/verify :sysadmin
    (if (= id (me))
      (bad-request "You can't delete yourself")
      (do (users/delete-user! id)
          (respond (pages/flash (str "Deleted " id)))))))

(defn- default-user-auth [] (name (or (config :default-user-auth) :unlimited)))

(defn- new-user-dialog [_]
  (auth/verify :sysadmin
    (respond (pages/new-user-dialog {:auth (default-user-auth) :send-email true}))))

(defn- create-user! [{:keys [params]}]
  (auth/verify :sysadmin
    (let [email      (some-> (param params :email) string/lower-case)
          user-name  (param params :name)
          auth       (keyword (:auth params))
          send-email (= "true" (:send-email params))
          error      (cond
                       (not (and email (string/includes? email "@"))) "Enter an email address."
                       (not user-name) "Enter a name."
                       (not (users/user-auth-types auth)) "Choose an authority."
                       ;; this makes the user, if they don't exist
                       (not (users/create-user! email user-name)) (str email " already has an account."))]
      (if error
        (respond (pages/new-user-dialog {:email email :name user-name :auth (:auth params)
                                         :send-email send-email :error error}))
        (do (users/set-user-auth! email auth)
            (when send-email (users/emit-reset-token! email))
            (-> (respond (pages/flash (str "Created " email
                                           (when send-email ", and emailed them a link to set their password"))))
                (with-trigger "close-modal, users-changed")))))))

;; Projects

(defn- project-filters [params]
  {:search  (param params :q)
   :orphans (= "true" (:orphans params))})

(defn- projects-tab [{:keys [params] :as request}]
  (auth/verify :sysadmin
    (let [filters (project-filters params)]
      (tab-response request (pages/projects-tab filters (admin-db/project-summaries filters))))))

(defn- projects-table [{:keys [params]}]
  (auth/verify :sysadmin
    (respond (pages/project-table (admin-db/project-summaries (project-filters params))))))

(defn- members-dialog
  ([project-id] (members-dialog project-id nil))
  ([project-id error]
   (when-let [project (admin-db/project-summary project-id)]
     (pages/members-dialog project (admin-db/project-members project-id)
                           (admin-db/user-ids) error))))

(defn- project-members [{{:keys [project-id]} :params}]
  (auth/verify :sysadmin
    (if-let [dialog (members-dialog project-id)]
      (respond dialog)
      (response/not-found "No such project"))))

(defn member-change-error
  "Why `op` (add, set or remove) on `user`, giving them `auth`, shouldn't
  happen to a project with `members`, or nil if it's OK. A project can't
  lose its last admin."
  [members op user auth user-exists?]
  (let [admins (set (for [m members :when (= "admin" (:auth m))] (:id m)))]
    (cond
      (not (#{"add" "set" "remove"} op)) "Unknown change."
      (not user) "Choose a user."
      (and (= "add" op) (not user-exists?)) (str "There is no user " user ".")
      (and (not= "remove" op) (not (#{:read :write :admin} auth))) "Choose a role."
      (and (= admins #{user}) (or (= "remove" op) (not= :admin auth)))
      "A project needs at least one admin, so add another first.")))

(defn- change-members! [{{:keys [project-id op auth] :as params} :params}]
  (auth/verify :sysadmin
    (let [user  (some-> (param params :user) string/lower-case)
          auth  (keyword auth)
          error (member-change-error (admin-db/project-members project-id) op user auth
                                     (and user (admin-db/user-summary user)))]
      (when-not error
        (case op
          ("add" "set") (users/authorize! project-id [{:id user :auth auth}])
          "remove"      (users/uninvite! project-id [user])))
      (-> (respond (members-dialog project-id error))
          (with-trigger "projects-changed")))))

(defn- delete-project! [{{:keys [project-id]} :params}]
  (auth/verify :sysadmin
    (let [project (admin-db/project-summary project-id)]
      (projects/delete-project! project-id)
      (project-lidar/delete-project-lidar! project-id)
      (respond (pages/flash (str "Deleted the project " (:name project)))))))

;; Jobs

(def ^:private jobs-per-page 50)

(defn job-filters
  "Which jobs to show, from the request's params"
  [params]
  {:queue    (param params :queue)
   :state    (some (set admin-db/job-states) [(param params :state)])
   :user     (param params :user)
   :page     (max 0 (or (some-> (param params :page) parse-long) 0))
   :per-page jobs-per-page})

(defn- jobs [{:keys [page per-page] :as filters}]
  (admin-db/jobs (assoc filters :offset (* page per-page) :limit per-page)))

(defn- jobs-tab [{:keys [params] :as request}]
  (auth/verify :sysadmin
    (let [filters (job-filters params)
          queues  (sort (distinct (concat (keys (admin-db/queue-counts)) (keys (capacities)))))]
      (tab-response request (pages/jobs-tab queues filters (jobs filters))))))

(defn- jobs-table [{:keys [params]}]
  (auth/verify :sysadmin
    (let [filters (job-filters params)]
      (respond (pages/job-table (jobs filters) filters)))))

(defn- view-job [{{:keys [job-id]} :params :as request}]
  (auth/verify :sysadmin
    (if-let [job (admin-db/job job-id)]
      (let [details (queue/job-details job-id)
            timing  (when (and (= "problems" (:queue-name job))
                               (#{"ready" "running"} (:state job)))
                      (queue/job-timing job-id))]
        (-> (if (htmx-request? request)
              (fragment (pages/job-dialog job details timing))
              (html (pages/job-page job details timing)))
            (cache-control/no-store)))
      (response/not-found "No such job"))))

(defn- act-on-job! [{{:keys [action job-id]} :params :as request}]
  {:pre [(int job-id) (#{"restart" "cancel"} action)]}
  (auth/verify [(keyword action) :job job-id]
    (case action
      "restart" (queue/restart job-id)
      "cancel"  (queue/cancel job-id))
    (if (htmx-request? request)
      (-> {:status 204} (with-trigger "jobs-changed, close-modal"))
      (response/redirect (str "/admin/job/" job-id) :see-other))))

(def admin-routes
  ["/admin"
   [[""          overview]
    ["/users"    {""       {:get users-tab :post create-user!}
                  "/table" users-table
                  "/new"   new-user-dialog}]
    ["/user"     {""       {:get user-dialog :delete delete-user!}
                  "/auth"  {:post set-user-auth!}
                  "/reset" {:post reset-password!}}]
    ["/projects" {""       projects-tab
                  "/table" projects-table}]
    [["/project/" [long :project-id]]
     {""         {:delete delete-project!}
      "/members" {:get project-members :post change-members!}}]
    ["/jobs"     {""       jobs-tab
                  "/table" jobs-table}]
    [["/job/" [long :job-id]] {:get view-job :post act-on-job!}]
    [["/queue/" :queue-name "/clean"] {:post clean-queue!}]
    [["/queue/" :queue-name "/purge"] {:post purge-queue!}]
    ["/tile-cache/clear" {:post clear-tile-cache!}]
    ["/email"      {:get email-dialog :post send-email!}]
    ["/map-bounds" get-map-bounds]]])

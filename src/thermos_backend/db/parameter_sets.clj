;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.db.parameter-sets
  "Parameter sets: named collections of the cost and other parameters
  from a network document (see `thermos-specs.parameters`), which can
  be applied to networks.

  A set has a scope:
  - :system sets everyone can see and use, but only sysadmins change
  - :user sets belong to one user
  - :project sets belong to a project, and its members share them

  Their content is EDN, which is migrated when read like a network's
  (see `thermos-backend.content-migrations.piecewise`). Content
  migrations must therefore cope with documents which only have some
  of their keys."
  (:require [thermos-backend.db :as db]
            [thermos-backend.content-migrations.piecewise :as piecewise]
            [thermos-specs.parameters :as parameters]
            [honeysql.helpers :as h]
            [honeysql.core :as sql]
            [jdbc.core :as jdbc]
            [clojure.string :as string]
            [clojure.edn :as edn]))

(def scopes #{:system :user :project})

(defn- as-scope [s]
  {:pre [(scopes s)]}
  (sql/call :parameter_scope (name s)))

(defn- categories->string [categories]
  (string/join " " (map name (filter (set categories) parameters/categories))))

(defn- string->categories [s]
  (set (map keyword (remove string/blank? (string/split (or s "") #" ")))))

(defn- read-content [{:keys [version content] :as row}]
  (let [doc (edn/read-string content)]
    (cond-> doc
      (< (or version 0) piecewise/current-version)
      (piecewise/migrate (or version 0)))))

(defn- tidy-row [row]
  (when row
    (cond-> (-> row
                (update :scope keyword)
                (update :categories string->categories))
      (:content row) (assoc :content (read-content row)))))

(def ^:private summary-columns
  [:parameter-sets.id :parameter-sets.name :parameter-sets.description
   :parameter-sets.scope :parameter-sets.user-id :parameter-sets.project-id
   :parameter-sets.categories :parameter-sets.revision
   :parameter-sets.is-system-default
   :parameter-sets.created :parameter-sets.updated
   [:projects.name :project-name]])

(defn- visible-to [user-id]
  [:or
   [:= :parameter-sets.scope (as-scope :system)]
   [:and
    [:= :parameter-sets.scope (as-scope :user)]
    [:= :parameter-sets.user-id user-id]]
   [:and
    [:= :parameter-sets.scope (as-scope :project)]
    [:in :parameter-sets.project-id
     (-> (h/select :project-id)
         (h/from :users-projects)
         (h/where [:= :user-id user-id]))]]])

(defn visible-sets
  "Summaries (no content) of the sets `user-id` can use, optionally only
  those which are system sets, the user's, or for `project-id`."
  [user-id & {:keys [project-id]}]
  (->> (-> (apply h/select summary-columns)
           (h/from :parameter-sets)
           (h/left-join :projects [:= :projects.id :parameter-sets.project-id])
           (h/where (visible-to user-id)
                    (when project-id
                      [:or
                       [:not= :parameter-sets.scope (as-scope :project)]
                       [:= :parameter-sets.project-id project-id]]))
           (h/order-by :parameter-sets.scope :projects.name :parameter-sets.name)
           (db/fetch!))
       (map tidy-row)))

(defn get-set
  "A set, with its :content unless `include-content` is false."
  ([id] (get-set id true))
  ([id include-content]
   (-> (apply h/select summary-columns)
       (cond-> include-content (h/merge-select :parameter-sets.content :parameter-sets.version))
       (h/from :parameter-sets)
       (h/left-join :projects [:= :projects.id :parameter-sets.project-id])
       (h/where [:= :parameter-sets.id id])
       (db/fetch-one!)
       (tidy-row))))

(defn current-revisions
  "For the sets `ids` which `user-id` can see, {id {:revision :name}}"
  [user-id ids]
  (if (empty? ids)
    {}
    (->> (-> (h/select :id :name :revision)
             (h/from :parameter-sets)
             (h/where [:in :id (vec ids)] (visible-to user-id))
             (db/fetch!))
         (into {} (map (juxt :id #(select-keys % [:revision :name])))))))

(defn create-set!
  "Store a new set, whose `content` is a partial document. Returns its ID."
  ([set] (db/with-connection [conn] (create-set! set conn)))
  ([{:keys [name description scope user-id project-id created-by content]} conn]
   {:pre [(string? name) (scopes scope)
          (or (not= :user scope) user-id)
          (or (not= :project scope) project-id)
          (map? content)]}
   (db/insert-one!
    :parameter-sets
    {:name name
     :description (or description "")
     :scope (as-scope scope)
     :user-id (when (= :user scope) user-id)
     :project-id (when (= :project scope) project-id)
     :created-by created-by
     :content (pr-str content)
     :categories (categories->string (parameters/categories-in content))
     :version piecewise/current-version}
    conn)))

(defn update-set!
  "Change a set's :name, :description, or :content. Changing the
  content increases its revision."
  [id {:keys [name description content]}]
  (-> (h/update :parameter-sets)
      (h/sset (cond-> {:updated (sql/call :now)}
                name        (assoc :name name)
                description (assoc :description description)
                content     (assoc :content (pr-str content)
                                   :categories (categories->string (parameters/categories-in content))
                                   :version piecewise/current-version
                                   :revision (sql/raw "revision + 1"))))
      (h/where [:= :id id])
      (db/execute!)))

(defn delete-set! [id]
  (-> (h/delete-from :parameter-sets)
      (h/where [:= :id id])
      (db/execute!)))

(defn copy-set!
  "Copy set `id` into a new scope; returns the new ID."
  [id {:keys [name scope user-id project-id created-by]}]
  (let [{:keys [content] :as original} (get-set id)]
    (create-set! {:name (or name (:name original))
                  :description (:description original)
                  :scope scope
                  :user-id user-id
                  :project-id project-id
                  :created-by created-by
                  :content content})))

(defn user-projects
  "The projects `user-id` is in, with their :name, the user's :auth, and
  the project's :default-parameter-set-id"
  [user-id]
  (->> (-> (h/select :projects.id :projects.name :users-projects.auth
                     :projects.default-parameter-set-id)
           (h/from :projects)
           (h/join :users-projects [:= :users-projects.project-id :projects.id])
           (h/where [:= :users-projects.user-id user-id])
           (h/order-by :projects.name)
           (db/fetch!))
       (map #(update % :auth keyword))))

;; Defaults

(defn set-user-default!
  "Make set `set-id` the default for `user-id` in `project-id`, or
  everywhere if `project-id` is nil. A nil `set-id` removes the default."
  [user-id project-id set-id]
  (db/with-connection [conn]
    (jdbc/atomic conn
      (-> (h/delete-from :parameter-set-defaults)
          (h/where [:= :user-id user-id]
                   (if project-id
                     [:= :project-id project-id]
                     [:= :project-id nil]))
          (db/execute! conn))
      (when set-id
        (-> (h/insert-into :parameter-set-defaults)
            (h/values [{:user-id user-id :project-id project-id :parameter-set-id set-id}])
            (db/execute! conn))))))

(defn user-defaults
  "{project-id-or-nil set-id} for `user-id`"
  [user-id]
  (->> (-> (h/select :project-id :parameter-set-id)
           (h/from :parameter-set-defaults)
           (h/where [:= :user-id user-id])
           (db/fetch!))
       (into {} (map (juxt :project-id :parameter-set-id)))))

(defn set-project-default! [project-id set-id]
  (-> (h/update :projects)
      (h/sset {:default-parameter-set-id set-id})
      (h/where [:= :id project-id])
      (db/execute!)))

(defn project-default [project-id]
  (-> (h/select :default-parameter-set-id)
      (h/from :projects)
      (h/where [:= :id project-id])
      (db/fetch-one!)
      :default-parameter-set-id))

(defn set-system-default!
  "Make system set `set-id` the default for everyone, or nil for none."
  [set-id]
  (db/with-connection [conn]
    (jdbc/atomic conn
      (-> (h/update :parameter-sets)
          (h/sset {:is-system-default false})
          (h/where [:= :is-system-default true])
          (db/execute! conn))
      (when set-id
        (-> (h/update :parameter-sets)
            (h/sset {:is-system-default true})
            (h/where [:= :id set-id] [:= :scope (as-scope :system)])
            (db/execute! conn))))))

(defn system-default []
  (-> (h/select :id)
      (h/from :parameter-sets)
      (h/where [:= :is-system-default true] [:= :scope (as-scope :system)])
      (db/fetch-one!)
      :id))

(defn default-choice
  "[set-id reason] for the set a new network in `project-id` made by
  `user-id` starts with, if any: the user's choice for the project
  (:mine-in-project), the project's default (:project), the user's own
  default (:mine), or the system default (:system), whichever comes
  first and is a set the user can still see."
  [user-id project-id]
  (let [user-choices (user-defaults user-id)
        choices (filter first [(when project-id [(get user-choices project-id) :mine-in-project])
                               (when project-id [(project-default project-id) :project])
                               [(get user-choices nil) :mine]
                               [(system-default) :system]])
        visible (if (seq choices)
                  (set (keys (current-revisions user-id (map first choices))))
                  #{})]
    (first (filter (comp visible first) choices))))

(defn default-set-id
  "The ID of the set a new network starts with; see `default-choice`"
  [user-id project-id]
  (first (default-choice user-id project-id)))

(defn project-info
  "A project's :id, :name and :default-parameter-set-id"
  [project-id]
  (-> (h/select :id :name :default-parameter-set-id)
      (h/from :projects)
      (h/where [:= :id project-id])
      (db/fetch-one!)))

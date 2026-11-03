;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.db.admin
  "Queries for the admin pages, which look across all users, projects and jobs."
  (:require [thermos-backend.db :as db]
            [clojure.string :as string]))

(def list-limit
  "Most users or projects to list at once; search to see others"
  500)

(defn- like-pattern
  "A LIKE pattern matching lower-cased text containing `search`"
  [search]
  (str "%" (-> search string/trim string/lower-case
               (string/replace #"[\\%_]" "\\\\$0"))
       "%"))

(defn- searching? [search] (not (string/blank? search)))

(def ^:private user-summary-sql
  "SELECT u.id, u.name, u.auth::text AS auth, u.password IS NOT NULL AS has_password,
          u.login_count, u.created, u.system_messages, u.job_emails,
          (SELECT count(*) FROM users_projects up WHERE up.user_id = u.id) AS project_count,
          (SELECT count(*) FROM networks n JOIN jobs j ON n.job_id = j.id
            WHERE n.user_id = u.id
              AND j.queued > (now() at time zone 'utc') - interval '7 days') AS recent_jobs
   FROM users u ")

(defn user-summaries
  "Users with some statistics, whose id or name contain `search` if given"
  [search]
  (db/fetch!
   (if (searching? search)
     (let [p (like-pattern search)]
       [(str user-summary-sql
             "WHERE lower(u.id) LIKE ? OR lower(u.name) LIKE ? ORDER BY u.id LIMIT ?")
        p p list-limit])
     [(str user-summary-sql "ORDER BY u.id LIMIT ?") list-limit])))

(defn user-summary [user-id]
  (db/fetch-one! [(str user-summary-sql "WHERE u.id = ?") user-id]))

(defn user-ids []
  (map :id (db/fetch! ["SELECT id FROM users ORDER BY id"])))

(defn user-projects
  "The projects `user-id` is in, with their authority on each"
  [user-id]
  (db/fetch!
   ["SELECT p.id, p.name, up.auth::text AS auth
     FROM users_projects up JOIN projects p ON p.id = up.project_id
     WHERE up.user_id = ? ORDER BY p.name"
    user-id]))

(def ^:private project-summary-sql
  "SELECT p.id, p.name, p.description, p.public,
          (SELECT count(*) FROM maps m WHERE m.project_id = p.id) AS map_count,
          (SELECT count(DISTINCT (n.map_id, n.name)) FROM networks n JOIN maps m ON n.map_id = m.id
            WHERE m.project_id = p.id) AS network_count,
          (SELECT max(n.created) FROM networks n JOIN maps m ON n.map_id = m.id
            WHERE m.project_id = p.id) AS last_saved,
          (SELECT coalesce(json_agg(json_build_object('id', up.user_id, 'auth', up.auth)
                                    ORDER BY up.auth DESC, up.user_id),
                           '[]'::json)
           FROM users_projects up WHERE up.project_id = p.id) AS members
   FROM projects p ")

(defn- where
  "A WHERE clause and its parameters, from pairs of SQL condition and
  parameters, which are ANDed"
  [conditions]
  (when (seq conditions)
    (into [(str "WHERE " (string/join " AND " (map first conditions)) " ")]
          (mapcat rest conditions))))

(defn project-summaries
  "Projects with some statistics, newest first. If given `search`, those
  whose name, id or members match it; if `orphans`, those with no members."
  [{:keys [search orphans]}]
  (let [[where-sql & where-params]
        (where
         (cond-> []
           (searching? search)
           (conj (let [p (like-pattern search)]
                   ["(lower(p.name) LIKE ? OR p.id::text = ?
                      OR EXISTS (SELECT 1 FROM users_projects up
                                 WHERE up.project_id = p.id AND lower(up.user_id) LIKE ?))"
                    p (string/trim search) p]))
           orphans
           (conj ["NOT EXISTS (SELECT 1 FROM users_projects up WHERE up.project_id = p.id)"])))]
    (db/fetch!
     (-> [(str project-summary-sql where-sql "ORDER BY p.id DESC LIMIT ?")]
         (into where-params)
         (conj list-limit)))))

(defn project-summary [project-id]
  (db/fetch-one! [(str project-summary-sql "WHERE p.id = ?") project-id]))

(defn project-members [project-id]
  (db/fetch!
   ["SELECT up.user_id AS id, u.name, up.auth::text AS auth
     FROM users_projects up JOIN users u ON u.id = up.user_id
     WHERE up.project_id = ? ORDER BY up.auth DESC, up.user_id"
    project-id]))

(def job-states
  ["ready" "running" "completed" "failed" "cancel" "cancelling" "cancelled"])

(def ^:private job-sql
  "Jobs, with the network or map they are for, and so the project and user"
  "SELECT j.id, j.queue_name, j.state::text AS state, j.queued, j.started, j.updated,
          j.expected_runtime, j.progress,
          n.id AS network_id, n.name AS network_name, n.user_id,
          m.id AS map_id, m.name AS map_name,
          p.id AS project_id, p.name AS project_name
   FROM jobs j
   LEFT JOIN LATERAL (SELECT id, name, map_id, user_id FROM networks
                      WHERE job_id = j.id LIMIT 1) n ON true
   LEFT JOIN maps m ON m.id = coalesce(n.map_id, (SELECT id FROM maps WHERE job_id = j.id LIMIT 1))
   LEFT JOIN projects p ON p.id = m.project_id ")

(defn jobs
  "A page of jobs, newest first, on `queue`, in `state`, and for networks
  saved by a user whose id contains `user`, if given. Gets one more than
  `limit` so you can tell if there are more."
  [{:keys [queue state user offset limit]}]
  (let [[where-sql & where-params]
        (where
         (cond-> []
           queue (conj ["j.queue_name = ?" queue])
           state (conj ["j.state = ?::job_state" state])
           (searching? user) (conj ["lower(n.user_id) LIKE ?" (like-pattern user)])))]
    (db/fetch!
     (-> [(str job-sql where-sql "ORDER BY j.id DESC LIMIT ? OFFSET ?")]
         (into where-params)
         (conj (inc limit) offset)))))

(defn job [job-id]
  (db/fetch-one! [(str job-sql "WHERE j.id = ?") job-id]))

(defn queue-counts
  "{queue-name {state count}}"
  []
  (reduce
   (fn [a {:keys [queue-name state count]}] (assoc-in a [queue-name state] count))
   (sorted-map)
   (db/fetch! ["SELECT queue_name, state::text AS state, count(*) FROM jobs GROUP BY 1, 2"])))

(defn system-counts []
  (db/fetch-one!
   ["SELECT (SELECT count(*) FROM users) AS users,
            (SELECT count(*) FROM users
              WHERE created > (now() at time zone 'utc') - interval '30 days') AS new_users,
            (SELECT count(*) FROM users WHERE login_count > 0) AS logged_in_users,
            (SELECT count(*) FROM projects) AS projects,
            (SELECT count(*) FROM projects WHERE public) AS public_projects,
            (SELECT count(*) FROM projects p
              WHERE NOT EXISTS (SELECT 1 FROM users_projects up WHERE up.project_id = p.id))
              AS orphan_projects,
            (SELECT count(*) FROM maps) AS maps,
            (SELECT count(DISTINCT (map_id, name)) FROM networks) AS networks,
            (SELECT count(*) FROM networks) AS network_versions,
            (SELECT count(*) FROM parameter_sets) AS parameter_sets,
            (SELECT count(*) FROM push_subscriptions) AS push_subscriptions,
            pg_database_size(current_database()) AS db_size"]))

(defn users-by-auth
  "{auth count}"
  []
  (into {} (map (juxt :auth :count))
        (db/fetch! ["SELECT auth::text AS auth, count(*) FROM users GROUP BY 1"])))

(defn table-sizes
  "The `n` biggest tables, with their size in bytes and roughly how many rows"
  [n]
  (db/fetch!
   ["SELECT relname AS name, n_live_tup AS rows, pg_total_relation_size(relid) AS bytes
     FROM pg_stat_user_tables ORDER BY bytes DESC LIMIT ?"
    n]))

(defn clear-tile-cache!
  "Delete the cached density tiles, which are made again when wanted"
  []
  (db/execute! ["TRUNCATE tilecache, tilemaxima"]))

(defn system-message-recipient-count
  "How many users `users/send-system-message!` would email"
  []
  (:count (db/fetch-one!
           ["SELECT count(*) FROM users WHERE system_messages AND id LIKE '%@%'"])))

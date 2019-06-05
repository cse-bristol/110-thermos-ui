(ns thermos-backend.db.users
  (:require [honeysql.helpers :as h]
            [honeysql-postgres.helpers :as p]
            [buddy.hashers :as hash]
            [thermos-backend.db :as db]
            [clojure.string :as string]
            [jdbc.core :as jdbc]
            [honeysql.core :as sql]
            [thermos-backend.email :as email]))

(defn- as-project-auth [a]
  {:pre [(#{:admin :read :write} a)]}
  (sql/call :project_auth (name a)))

(defn- as-user-auth [a]
  {:pre [(#{:admin :normal} a)]}
  (sql/call :user_auth (name a)))

(defn- max-auth [a b]
  (cond
    (nil? a) b
    (nil? b) a
    (= :admin a) a
    (= :admin b) b
    (= :write a) a
    (= :write b) b
    :else b))

(defn users []
  (-> (h/select :*)
      (h/from :users)
      (h/order-by :auth :id)
      (db/fetch!)))

(defn user-rights
  "Given a `user-id` (email address), this will return a map describing their db rights, having at least.
  :name, :id, :auth - the user's name, id and system authority
  :projects - a map keyed on project-id, whose values have
     - :project-id
     - :auth - the user's authority on this project"
  [user-id]
  (when user-id
    (let [[user-details project-details]
          (db/with-connection [conn]
            [(-> (h/select :*)
                 (h/from :users)
                 (h/where [:= :id user-id])
                 (db/fetch! conn)
                 (first))

             (-> (h/select :*)
                 (h/from :users-projects)
                 (h/where [:= :user-id user-id])
                 (db/fetch! conn))
             ])]
      (-> user-details
          (update :auth keyword)
          (assoc 
             :projects
             (reduce
              (fn [a m] (assoc a (:project-id m) (update m :auth keyword)))
              {} project-details))))))

(defn delete-user! [user-id]
  (-> (h/delete-from :users)
      (h/where [:= :id user-id])
      (db/execute!)))

(defn create-user!
  "Create a user, if they do not exist.
  If they exist, returns nil
  If they do not exist, returns a string which is their password-reset token.
  "
  [email name & [password conn]]
  {:pre [(or (nil? password)
             (and (string? password)
                  (not (string/blank? password))))
         (and (string? email)
              (not (string/blank? email)))
         (string? name)]}
    (db/or-connection [conn]
    (jdbc/atomic
     conn
     (let [exists-already
           (-> (h/select [(sql/call
                           :exists
                           (-> (h/select :id)
                               (h/from :users)
                               (h/where [:= :id email])))
                          :exists])
               (db/fetch! conn)
               (first)
               (:exists))]
       (when-not exists-already
         (let [token (or (not (nil? password))
                         (str (java.util.UUID/randomUUID)))]
           (-> (h/insert-into :users)
               (h/values [{:id (string/lower-case email)
                           :name name
                           :reset-token (and (not password) token)
                           :password (and password (hash/derive password))
                           :auth (as-user-auth :normal)}])
               (db/execute! conn))
           token))))))

(defn correct-password? [user-id password]
  {:pre [(string? user-id)
         (string? password)]}
  (-> (h/select :password)
      (h/from :users)
      (h/where [:= :id user-id])
      (db/fetch!)
      (first)
      (:password)
      (->> (hash/check password))))

(defn logged-in! [user-id]
  {:pre [(string? user-id)]}
  (-> (h/update :users)
      (h/sset {:login-count (sql/call :+ :login-count 1)})
      (h/where [:= :id user-id])
      (db/execute!)))

(defn gen-reset-token!
  "Generate a reset token for `user-id` and return it.
  If there is no user for `user-id` return nil."
  [user-id]
  {:pre [(string? user-id)]}
  (let [new-token (str (java.util.UUID/randomUUID))
        n (-> (h/update :users)
              (h/sset {:reset-token new-token})
              (h/where [:= :id user-id])
              (db/execute!))]
    (when (and n (pos? n))
      new-token)))

(defn invite!
  [project-id current-user invitees & [conn]]
  {:pre [(int? project-id)]}
  ;; send invitations to all these users onto this project.
  (when (seq invitees)
    (db/or-connection [conn]
      (let [;; this is a bit nasty but project depends on user so we don't
            ;; want user to depend on project
            project-name (-> (h/select :name)
                             (h/from :projects)
                             (h/where [:= :id project-id])
                             (db/fetch! conn)
                             (first)
                             (:name))]
        (doseq [u invitees]
          (email/send-invitation-message
           (:id u) (:name current-user)
           project-name
           (create-user! (:id u) (:name u) nil conn))))
      
      (-> (h/insert-into :users-projects)
          (h/values (for [u invitees]
                      {:user-id (:id u)
                       :project-id project-id
                       :auth (as-project-auth (or (:auth u) :write))}))
          (db/execute! conn)))))

(defn uninvite!
  [project-id user-ids & [conn]]
  {:pre [(int? project-id)]}
  (when (seq user-ids)
    (db/or-connection [conn]
      (-> (h/delete-from :users-projects)
          (h/where [:and
                    [:= :project-id project-id]
                    [:in :user-id user-ids]])
          (db/execute! conn)))))

(defn authorize!
  "Update the authority of the given users on the project.
  If a user is repeated, their authority will be the maximum listed."
  [project-id users & [conn]]

  (let [the-values
        (for [[id entries] (group-by :id users)]
          {:project-id project-id
           :user-id id
           :auth (as-project-auth (reduce max-auth (map :auth entries)))})]
    
    (db/or-connection [conn]
      (-> (h/insert-into :users-projects)
          (h/values the-values)
          (p/upsert
           (-> (p/on-conflict :user-id :project-id)
               (p/do-update-set :auth)))
          (db/execute! conn)))))

(defn verify-reset-token
  "If `token` is a valid reset token for a user, return that user's ID"
  [token]
  (-> (h/select :id)
      (h/from :users)
      (h/where [:= :reset-token token])
      (db/fetch-one!)
      (:id)))

(defn emit-reset-token!
  "Generate a reset token for the given user, and send them an email.
  This is used for the user forgetting their password."
  [user-id]
  (when-let [token (gen-reset-token! user-id)]
    (email/send-password-reset-token user-id token)))

(defn update-user!
  "Set a user's name and password. If name or password are nil or blank, they are unchanged."
  [user-id new-name new-password system-messages]
  (-> (h/update :users)
      (h/sset (cond-> {}
                (not (string/blank? new-name))
                (assoc :name new-name)
                (not (string/blank? new-password))
                (assoc :password
                       (hash/derive new-password)
                       :reset-token nil)
                (not (nil? system-messages))
                (assoc :system-messages system-messages)))
      
      (h/where [:= :id user-id])
      (db/execute!)))

(defn send-system-message! [subject message]
  {:pre [(string? subject)
         (string? message)
         (not (string/blank? subject))
         (not (string/blank? message))]}
  
  (let [users (-> (h/select :id :name)
                  (h/from :users)
                  (h/where [:and
                            [:= :TRUE :system-messages]
                            [:like :id "%@%"]
                            ])
                  (db/fetch!))]
    (email/send-system-message users subject message)))


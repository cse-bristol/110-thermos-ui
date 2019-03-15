(ns thermos-backend.db.users
  (:require [honeysql.helpers :as h]
            [buddy.hashers :as hash]
            [thermos-backend.db :as db]
            [clojure.string :as string]
            [jdbc.core :as jdbc]
            [honeysql.core :as sql]
            [thermos-backend.email :as email]))

(defn users []
  (-> (h/select :*)
      (h/from :users)
      (db/fetch!)))

(defn user-rights [user-id]
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
      (assoc user-details
             :projects
             (reduce
              (fn [a m] (assoc a (:project-id m) m))
              {} project-details)))))

(defn delete-user! [user-id]
  (-> (h/delete-from :users)
      (h/where [:= :id user-id])
      (db/execute!)))

(defn create-user! [email name password]
  {:pre [(or (nil? password)
             (and (string? password)
                  (not (string/blank? password))))
         (and (string? email)
              (not (string/blank? email)))
         (string? name)]}
  (db/with-connection [conn]
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
                           :auth (sql/call :user_auth "normal")}])
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
  "If the user exists already, invite them onto the project.
  If they don't exist, create them and then invite them."
  [inviter
   {email :id name :name auth :auth}
   {project-id :id project-name :name}]
  
  (let [token (create-user! email name nil)]
    (email/send-invitation-message
     email
     inviter
     project-name
     token)))

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


(defn update-user! [user-id new-name new-password]
  (-> (h/update :users)
      (h/sset (cond-> {}
                (not (string/blank? new-name))
                (assoc :name new-name)
                (not (string/blank? new-password))
                (assoc :password
                       (hash/derive new-password)
                       :reset-token nil)))
      (h/where [:= :id user-id])
      (db/execute!)))


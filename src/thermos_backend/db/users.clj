(ns thermos-backend.db.users
  (:require [honeysql.helpers :as h]
            [buddy.hashers :as hash]
            [thermos-backend.db :as db]
            [clojure.string :as string]
            [jdbc.core :as jdbc]
            [honeysql.core :as sql]))

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
         (-> (h/insert-into :users)
             (h/values [{:id (string/lower-case email)
                         :name name
                         :password (hash/derive password)
                         :auth (sql/call :user_auth "normal")}])
             (db/execute! conn))
         true)))))

(defn correct-password? [user-id password]
  (-> (h/select :password)
      (h/from :users)
      (h/where [:= :id user-id])
      (db/fetch!)
      (first)
      (:password)
      (->> (hash/check password))))

(defn invite! [user project])

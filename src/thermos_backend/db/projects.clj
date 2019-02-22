(ns thermos-backend.db.projects
  (:require [thermos-backend.db :as db]
            [thermos-backend.db.users :as users]
            [honeysql.helpers :as h]
            [honeysql-postgres.helpers :as p]
            [honeysql-postgres.format]
            [clojure.string :as string]
            [honeysql.core :as sql]))

(defn get-project
  "Select project details for `project-id`.
  Gets everything from the projects table and the maps relation
  under :maps."
  [project-id]
  {:pre [(int? project-id)]}
  (-> (h/select :projects.*
                [(sql/call :json_agg (sql/call :to_json :maps.*)) :maps])
      (h/from :projects)
      (h/left-join :maps [:= :projects.id :maps.project-id])
      (h/group :projects.id)
      (h/where [:= :projects.id project-id])
      (db/fetch!)
      (first)))

(defn create-project!
  "Create a new project with the given `name` and `description`.

  The `users` are all given :write :auth to the project unless
  you say otherwise. You must supply at least one user with :admin
  authority.

  Any user who does not exist will be created and sent an invitation
  email."
  [project-name description users]
  {:pre
   [(string? project-name)
    (string? description)
    (every? (fn [{e :email n :name a :auth}]
              (and (string? e)
                   (string? n)
                   (contains? #{nil :admin :read :write} a)))
            users)
    (any? (fn [{a :auth}] (= a :admin)) users)
    ]}

  ;; TODO uniquify emails
  ;; TODO do this all in a transaction? does it really matter?
  
  ;; 1. create the project
  (let [project-name (string/trim project-name)
        description (string/trim description)
        
        project-id
        (db/insert-one! :projects {:name project-name :description description})

        project
        {:id project-id
         :name project-name
         :description description}
        ]
    ;; 2. ensure the users exist, and send them invitations if need be
    (run! #(users/invite! % project) users)

    ;; 3. give the users read-auth on the project
    (-> (h/insert-into :users-projects)
        (h/values (for [{e :email a :auth} users]
                    {:project-id project-id
                     :user-id e
                     :auth (sql/call :project_auth (name (or :read a)))}))
        (db/execute!))))

(defn user-projects
  "Return a list of the projects for `user-id`."
  [user-id]
  {:pre [(string? user-id)]}
  (-> (h/select :*)
      (h/from :projects)
      (h/join :users-projects
              [:= :projects.id :users-projects.project-id])
      (h/where [:= :users-projects.user-id user-id])
      (db/fetch!)))

(defn create-map!
  "Create a new map within the project."
  [project-id map-name description]
  {:pre [(int? project-id)
         (string? map-name)
         (string? description)
         (not (string/blank? map-name))]}

  (db/insert-one!
   :maps {:project-id project-id :name map-name}))


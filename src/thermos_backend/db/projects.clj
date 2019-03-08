(ns thermos-backend.db.projects
  (:require [thermos-backend.db :as db]
            [thermos-backend.db.users :as users]
            [thermos-backend.db.json-functions :as json]
            [honeysql.helpers :as h]
            [honeysql-postgres.helpers :as p]
            [clojure.tools.logging :as log]
            [honeysql-postgres.format]
            [clojure.string :as string]
            [jdbc.core :as jdbc]
            [honeysql.core :as sql]))

(defn- join-json [& fields]
  (sql/call
   :case
   [:=
    (sql/call :count (first fields))
    (sql/inline 0)]
   :null

   :else
   (sql/call
    :json_agg
    (if (some #(.contains (name %) "*")
             fields)
      (apply sql/call :to_json fields)
      (apply sql/call :json_build_object
             (flatten (for [f fields]
                        [(last (.split (name f) "\\.")) f])))))))


(comment
  (get-project 10)
  )

(defn get-project
  "Select project details for `project-id`.
  A bit of an omni-join which is not beautiful.
  
  Gets everything from the projects table and the maps relation
  under :maps, and then everything under :networks in each map"
  [project-id]
  {:pre [(int? project-id)]}
  (-> (h/select :projects.*
                [(join-json :maps.*) :maps]
                [(sql/call
                  :json_agg
                  (-> (h/select
                       (json/build-object
                        :id :users.id
                        :name :users.name
                        :auth :users-projects.auth))
                      
                      (h/from :users)
                      (h/join :users-projects
                              [:= :users.id
                               :users-projects.user-id])
                      (h/where [:= :users-projects.project-id
                                :projects.id])))
                 :users])
      
      (h/from :projects)
      (h/left-join
       [(-> (h/select :maps.*
                      :jobs.state
                      [(join-json :networks.id :networks.name) :networks])
            (h/from :maps)
            (h/left-join :networks
                         [:= :maps.id :networks.map-id]
                         :jobs
                         [:= :maps.job-id :jobs.id])
            
            (h/group :maps.id :jobs.state)) :maps]
       [:= :projects.id :maps.project-id])
      
      (h/group :projects.id)
      (h/where [:= :projects.id project-id])
      (db/fetch!)
      (first)))

(defn create-project!
  "Create a new project with the given `name` and `description`.

  The `creator` should have :id and :name, and is set as an admin of
  the project.
  
  The `users` are all given :write :auth to the project unless
  you say otherwise. They should have :id, :name and maybe :auth.

  Any user who does not exist will be created and sent an invitation
  email."
  [creator project-name description users]
  {:pre
   [(string? (:id creator))
    (string? (:name creator))
    (string? project-name)
    (string? description)
    (every? (fn [{e :id n :name a :auth}]
              (and (string? e)
                   (string? n)
                   (contains? #{nil :admin :read :write} a)))
            users)]}

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
    ;;    the creator should already exist.
    (run! #(users/invite! (:name creator) % project) users)

    ;; 3. give the users read-auth on the project
    (let [users (conj users (assoc creator :auth :admin))]
      (-> (h/insert-into :users-projects)
          (h/values (for [{e :id a :auth} users]
                      {:project-id project-id
                       :user-id e
                       :auth (sql/call :project_auth
                                       (name (or a :read)))}))
          (db/execute!)))

    project-id))

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

(defn get-network
  "Get a network problem for `network-id`"
  [network-id & {:keys [include-content]}]
  {:pre [(int? network-id)]}

  (-> (h/select :networks.map-id
                :networks.name
                :networks.created
                :networks.has-run
                :networks.job-id
                :ranked-jobs.state
                :ranked-jobs.queue-position)
      (cond-> include-content
        (h/merge-select :networks.content))
      (h/from :networks)
      (h/left-join :ranked-jobs [:= :networks.job-id :ranked-jobs.id])
      (h/where [:= :networks.id network-id])
      (db/fetch-one!)
      (update :state keyword)))

(defn save-network! [project-id map-id name content]
  {:pre [(int? project-id)
         (int? map-id)
         (string? name)
         (string? content)]}
  (db/insert-one!
   :networks
   {:map-id map-id
    :name name
    :content content}))

(defn add-solution! [network-id content]
  (-> (h/update :networks)
      (h/sset {:content content :has-run true})
      (h/where [:= :id network-id])
      (db/execute!)))

(defn associate-job! [network-id job-id]
  (-> (h/update :networks)
      (h/sset {:job-id job-id})
      (h/where [:= :id network-id])
      (db/execute!)))

(defn delete-project! [project-id]
  (-> (h/delete-from :projects)
      (h/where [:= :id project-id])
      (db/execute!)))

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
            [honeysql.core :as sql]
            [com.rpl.specter :refer :all]))

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
    (if (some #(and (keyword? %)
                    (.contains (name %) "*"))
             fields)
      (apply sql/call :to_json fields)
      (apply sql/call :json_build_object
             (flatten (for [f fields]
                        [(cond
                           (keyword? f)
                           [(last (.split (name f) "\\.")) f]
                           
                           (vector? f)
                           [(name (second f)) (first f)])
                         
                         ])))))))

(defn get-project
  "Select project details for `project-id`.
  A bit of an omni-join which is not beautiful.
  
  Gets everything from the projects table and the maps relation
  under :maps, and then everything under :networks in each map"
  [project-id]
  {:pre [(int? project-id)]}
  (-> (h/select :projects.*
                [(join-json :maps.*) :maps]
                [(-> (h/select
                      
                      (json/agg
                       (json/build-object
                        :id :users.id
                        :name :users.name
                        :auth :users-projects.auth)))
                     
                     (h/from :users)
                     (h/join :users-projects
                             [:= :users.id
                              :users-projects.user-id])
                     (h/where [:= :users-projects.project-id
                               :projects.id]))
                 :users])
      
      (h/from :projects)
      (h/left-join
       [(-> (h/select :maps.*
                      :jobs.state
                      [(join-json :networks.id
                                  :networks.name
                                  :networks.user-id
                                  [:users.name :user-name]
                                  :networks.created
                                  :networks.has-run
                                  :networks.job-id)
                       :networks])
            (h/from :maps)
            (h/left-join :networks
                         [:= :maps.id :networks.map-id]
                         :jobs
                         [:= :maps.job-id :jobs.id])
            (h/merge-left-join :users [:= :networks.user-id :users.id])
            
            (h/group :maps.id :jobs.state)) :maps]
       [:= :projects.id :maps.project-id])
      
      (h/group :projects.id)
      (h/where [:= :projects.id project-id])
      (db/fetch!)
      (first)

      ;; these are specter functions used to tidy up / group a bit.
      (->>
       (transform [:users ALL :auth] keyword)
       (transform [:maps ALL :networks] #(group-by :name %))
       (setval [:maps ALL :networks MAP-VALS ALL :name] NONE))))

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
    (when (seq users)
      (users/invite! project-id creator users))

    ;; 3. give the users read-auth on the project
    (let [users (conj users (assoc creator :auth :admin))]
      (users/authorize! project-id users))

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

(defn save-network! [user-id project-id map-id name content]
  {:pre [(int? project-id)
         (int? map-id)
         (string? name)
         (string? content)]}
  (db/insert-one!
   :networks
   {:map-id map-id
    :name name
    :user-id user-id
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

(defn set-users!
  "Set the authority of the given users for the given project.
  - `users` should be a list of maps having
    - :id - the user email address
    - :auth - the user's authority

  If a user does not exist, they will be created and sent an email
  If they do exist, but are not on the project, they will be put on and sent an email
  If they exist and are on the project their authority will be set, unless this leaves the project with no admin.
  If this is what would happen all the current admins are retained.
  Any users currently on the project who are not in the list will be removed, subject to the same proviso.
  "
  [project-id current-user users & [conn]]
  (db/or-connection [conn]
    (let [current-users (-> (h/select :*)
                            (h/from :users-projects)
                            (h/where [:= :project-id project-id])
                            (db/fetch! conn))

          any-admins? (some #{:admin} (map :auth users))
          existing-admins (filter (comp #{:admin} :auth) current-users)
          
          users (if any-admins?
                  users
                  (concat users existing-admins))
          
          desired-user-ids (set (map :id users))
          current-user-ids (set (map :user-id current-users))
          
          users-to-invite (remove (comp current-user-ids :id) users)
          users-to-remove (remove (comp desired-user-ids :user-id) current-users)
          ]
      (log/info "Remove" users-to-remove "from" project-id)
      (log/info "Invite" users-to-invite "to" project-id)
      (log/info "Authorize" users "for" project-id)
      (jdbc/atomic
       conn
       (users/uninvite! project-id (map :user-id users-to-remove) conn)
       (users/invite! project-id current-user users-to-invite conn)
       (users/authorize! project-id users conn)))))


(defn delete-networks! [map-id network-name]
  (-> (h/delete-from :networks)
      (h/where [:and
                [:= :map-id map-id]
                [:= :name network-name]])
      (db/execute!)))

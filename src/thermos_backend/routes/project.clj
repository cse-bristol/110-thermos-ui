;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.routes.project
  (:require [thermos-backend.routes.responses :refer :all]
            [thermos-backend.pages.projects :as project-pages]
            [thermos-backend.auth :as auth]
            [clojure.string :as string]
            [ring.util.response :as response]
            [ring.util.codec :refer [url-decode]]
            [thermos-backend.db.projects :as projects]
            [thermos-backend.pages.cache-control :as cache-control]
            [thermos-backend.pages.maps :as map-pages]
            [thermos-backend.pages.editor :as editor]
            [thermos-backend.db.maps :as maps]
            [thermos-backend.db.users :as users]
            [thermos-backend.config :refer [config]]
            [thermos-backend.restrictions :as restrictions]
            [clojure.java.io :as io]
            [ring.util.io :as ring-io]
            [cheshire.core :as json]
            [clojure.tools.logging :as log]
            [thermos-util.converter :as converter]
            [clojure.edn :as edn]
            [thermos-backend.solver.core :as solver]
            [thermos-backend.queue :as queue]
            [thermos-backend.routes.lidar :as lidar-routes]
            [thermos-backend.routes.map-wizard :as map-wizard]
            [thermos-backend.project-lidar :as project-lidar])
  (:import [javax.mail.internet InternetAddress]
           [java.io ByteArrayInputStream]))

(defn- parse-email [text]
  (try
    (let [address (InternetAddress. text)]
      {:id    (string/trim (string/lower-case (.getAddress address)))
       :name  (or (.getPersonal address)
                  (.getAddress address))})
    (catch javax.mail.internet.AddressException e)))

(defn- parse-emails [text]
  (keep (comp parse-email string/trim)
        (string/split text #"[\n,;]+")))

(defn- normalize-users [users]
  (->> users
       (mapcat
        (fn [user]
          (let [id (:id user) emails (parse-emails id)]
            (if (seq emails)
              (for [e emails] (merge user e))
              [user]))))
       (reduce (fn [out user] (assoc out (:id user) user)) {})
       (vals)))

(defn- new-project-page [_]
  (auth/verify :logged-in
    (html (project-pages/new-project-page))))

(defn- create-project! [{{:keys [name description members]} :params}]
  (auth/verify :logged-in
    (let [members (parse-emails members)
          new-project-id (projects/create-project!
                          auth/*current-user*
                          name description members)]
      (response/redirect (str "./" new-project-id)))))

(defn- get-project-data [id]
  (let [project (projects/get-project id)
        user-auth (->> (:users project)
                       (filter (comp #{(:id auth/*current-user*)} :id))
                       (first)
                       (:auth))
        ]
    (-> project
        (assoc :user-is-admin
               (= :admin user-auth))
        (assoc :user-auth user-auth)
        (assoc :user (:id auth/*current-user*))
        (assoc :restriction-info (restrictions/get-restriction-info auth/*current-user* id)))))

(defn- project-page [{{:keys [project-id expanded]} :params :as req}]
  (auth/verify [:read :project project-id]
    (-> (project-pages/project-page (get-project-data project-id) expanded)
        (html)
        (cache-control/no-store))))

(defn- maps-fragment
  "The list of maps in the project, for htmx to swap in"
  [project-id expanded]
  (-> (project-pages/project-maps (get-project-data project-id) expanded)
      (fragment)
      (cache-control/no-store)))

(defn- project-maps [{{:keys [project-id expanded]} :params}]
  (auth/verify [:read :project project-id]
    (maps-fragment project-id expanded)))

(defn- leave-project-dialog [{{:keys [project-id]} :params}]
  (auth/verify [:leave :project project-id]
    (-> (project-pages/leave-project-form (get-project-data project-id))
        (fragment)
        (cache-control/no-store))))

(defn- leave-project! [{{:keys [project-id]} :params}]
  (auth/verify [:leave :project project-id]
    (projects/leave! project-id (:id auth/*current-user*))
    (response/redirect "/" :see-other)))

(defn- delete-project! [{{:keys [project-name project-id]} :params
                         method :request-method}]
  (auth/verify [:delete :project project-id]
    (cond (or (= :delete method)
              (and (= :post method)
                   (= (string/lower-case project-name)
                      (string/lower-case (:name (projects/get-project project-id))))))
          (do (projects/delete-project! project-id)
              (project-lidar/delete-project-lidar! project-id)
              (if (= :delete method)
                deleted
                (response/redirect "/" :see-other)))
          
          (= :post method)
          (response/redirect (str "/project/" project-id "/delete?wrong-name=1") :see-other))))

(defn- delete-project-page [{{:keys [project-id wrong-name]} :params :as request}]
  (auth/verify [:delete :project project-id]
    (let [project (projects/get-project project-id)]
      (-> (if (htmx-request? request)
            (fragment (project-pages/delete-project-form project wrong-name))
            (html (project-pages/delete-project-page project wrong-name)))
          (cache-control/no-store)))))

(defn- rename-project-dialog [{{:keys [project-id]} :params}]
  (auth/verify [:rename :project project-id]
    (-> (project-pages/rename-project-form (projects/get-project project-id))
        (fragment)
        (cache-control/no-store))))

(defn- rename-project! [{{:keys [project-id name description]} :params}]
  (auth/verify [:rename :project project-id]
    (when-not (string/blank? name)
      (projects/rename-project! project-id (string/trim name) (some-> description string/trim)))
    (response/redirect (str "/project/" project-id) :see-other)))

(defn- users-from-form
  "The form for project users has fields id:<i>, name:<i>, auth:<i> and new:<i>
  for each user; this gets them back out in order."
  [params]
  (let [params (into {} (for [[k v] params] [(name k) v]))]
    (->> (keys params)
         (keep #(second (re-matches #"id:(\d+)" %)))
         (sort-by #(Integer/parseInt %))
         (mapv (fn [i]
                 (cond-> {:id   (params (str "id:" i))
                          :name (params (str "name:" i))
                          :auth (keyword (params (str "auth:" i) "read"))}
                   (params (str "new:" i)) (assoc :new true)))))))

(defn- project-users-dialog [{{:keys [project-id]} :params}]
  (auth/verify [:share :project project-id]
    (let [project (projects/get-project project-id)]
      (-> (project-pages/project-users-form project-id (:users project) (:public project))
          (fragment)
          (cache-control/no-store)))))

(defn- edit-project-users
  "Make a change to the project users form, without saving it."
  [{{:keys [project-id op invite public] :as params} :params}]
  (auth/verify [:share :project project-id]
    (let [users (users-from-form params)
          [op i] (string/split (str op) #":")
          i      (and i (Integer/parseInt i))
          users  (case op
                   "add"     (let [existing (set (map :id users))]
                               (into users
                                     (for [e (parse-emails (str invite))
                                           :when (not (existing (:id e)))]
                                       (assoc e :auth :read :new true))))
                   "promote" (assoc-in users [i :auth] :admin)
                   "demote"  (assoc-in users [i :auth] :read)
                   "remove"  (into (subvec users 0 i) (subvec users (inc i)))
                   users)]
      (-> (project-pages/project-users-form project-id users (boolean public))
          (fragment)
          (cache-control/no-store)))))

(defn- set-project-users! [{{:keys [public project-id] :as params} :params}]
  (let [users (normalize-users (map #(dissoc % :new) (users-from-form params)))]
    (auth/verify [:share :project project-id]
      (projects/set-users! project-id auth/*current-user* users)
      (projects/make-public! project-id (boolean public))
      (response/redirect (str "/project/" project-id) :see-other))))

(defn- map-icon [{{:keys [map-id]} :params}]
  (auth/verify [:read :map map-id]
    (some-> (maps/get-icon map-id)
            (ByteArrayInputStream.)
            (response/response)
            (response/content-type "image/png"))))

(defn- delete-map! [{method :request-method {:keys [map-id]} :params}]
  (auth/verify [:delete :map map-id]
    (when (#{:delete :post} method)
      (maps/delete-map! map-id)
      (cond (= :delete method) deleted
            (= :post method) (response/redirect "../../")))))

(defn- project-map
  "The map from the project data, which has what the project page shows"
  [project-id map-id]
  (->> (:maps (projects/get-project project-id))
       (filter (comp #{map-id} :id))
       (first)))

(defn- rename-map-dialog [{{:keys [map-id project-id]} :params}]
  (auth/verify [:write :map map-id]
    (-> (project-pages/rename-map-form (project-map project-id map-id))
        (fragment)
        (cache-control/no-store))))

(defn- rename-map! [{{:keys [map-id project-id name]} :params}]
  (auth/verify [:write :map map-id]
    (when-not (string/blank? name)
      (maps/rename-map! map-id (string/trim name)))
    (response/redirect (str "/project/" project-id) :see-other)))

(defn- delete-map-page [{{:keys [map-id project-id]} :params :as request}]
  (auth/verify [:delete :map map-id]
    (let [map-info (project-map project-id map-id)]
      (-> (if (htmx-request? request)
            (fragment (project-pages/delete-map-form map-info))
            (html (map-pages/delete-map-page map-info)))
          (cache-control/no-store)))))

(defn- streaming-map [map-id]
  (ring-io/piped-input-stream
   (fn [o]
     (with-open [w (io/writer o)]
       (doto w
         (.write "{ \"type\": \"FeatureCollection\", ")
         (.write "  \"features\": ["))

       
       (try
         (let [first (atom true)]
          (maps/stream-features
           map-id
           (fn [feature]
             (if @first (reset! first false) (.write w ","))

             (json/encode-stream
              {:type :Feature
               :geometry (json/decode (:geometry feature))
               :properties (dissoc feature :geometry)}
              w)
             
             (.write w "\n"))
           ))
         (catch Exception e
           (log/error e "Whilst streaming" map-id)))

       (.write w "]}")
       (.flush w))
     (.flush o))))

(defn- attachment-disposition [r filename]
  (-> r (response/header
         "Content-Disposition"
         (str "attachment"
              (when filename
                (str "; filename=\"" filename "\""))))))

(defn- map-geojson-data [{{:keys [map-id]} :params}]
  (auth/verify [:read :map map-id]
    (-> (response/response (streaming-map map-id))
        (response/content-type "application/json")
        (attachment-disposition (str (:name (maps/get-map map-id)) ".json")))))

(defn- map-vector-tile [{{:keys [map-id z x y]} :params}]
  (auth/verify [:read :map map-id]
    (json (maps/get-tile map-id z x y))))

(defn- map-heat-density-tile [{{:keys [map-id z x y]} :params}]
  (auth/verify [:read :map map-id]
    (-> (maps/get-density-tile map-id z x y true)
        (ByteArrayInputStream.)
        (response/response)
        (response/content-type "image/png"))))

(defn- map-cold-density-tile [{{:keys [map-id z x y]} :params}]
  (-> (maps/get-density-tile map-id z x y false)
      (ByteArrayInputStream.)
      (response/response)
      (response/content-type "image/png")))

(defn- new-network-page [{{:keys [mode map-id project-id]} :params}]
  (auth/verify [:read :map map-id]
    (-> (editor/editor-page nil
                            nil
                            (if (= mode "cooling")
                              :cooling
                              :heating)
                            (maps/get-map-bounds map-id)
                            (nil? (projects/get-map-project-auth
                                   map-id
                                   (:id auth/*current-user*)))
                            (restrictions/get-restriction-info auth/*current-user* project-id))
        (response/response)
        (response/status 200)
        (response/content-type "text/html"))))

(defn- network-editor-page [{{:keys [net-id project-id]} :params {accept "accept"} :headers}]
  (auth/verify [:read :network net-id]
    (let [accept (set (string/split accept #","))
          info   (projects/get-network net-id :include-content true)
          project-auth (projects/get-network-project-auth
                        net-id
                        (:id auth/*current-user*))]
      (-> (cond
            (accept "application/edn")
            (-> (response/response (:content info))
                (response/status 200)
                (response/content-type "text/edn"))
            
            (or (accept "*/*")
                (accept "text/html"))
            (-> (editor/editor-page (:name info)
                                    (:content info)
                                    nil
                                    nil
                                    (nil? project-auth)
                                    (restrictions/get-restriction-info auth/*current-user* project-id))
                
                (response/response)
                (response/status 200)
                (response/content-type "text/html")))
          
          (cache-control/no-store)
          (response/header "X-Queue-Position" (:queue-position info))
          (response/header "X-Name" (:name info))
          (response/header "X-Run-State" (:state info))))))

(defn- network-poll-status [{{:keys [net-id]} :params}]
  (auth/verify [:read :network net-id]
    (let [info (projects/get-network net-id)
          info (cond-> info
                 (and (:job-id info)
                      (#{:ready :running} (:state info)))
                 (assoc :timing (queue/job-timing (:job-id info))))]
      (-> (transit-json info)
          (response/status 200)
          (response/header "X-Name" (:name info))
          (response/header "X-Queue-Position" (:queue-position info))
          (response/header "X-Run-State" (:state info))
          (cache-control/no-store)))))

(defn- network-save! [{{:keys [name run content map-id project-id]} :params}]
  {:pre [(contains? #{nil "network" "supply" "both" "tidy" "tree"} run)
         (int? map-id)
         (int? project-id)
         (string? name)
         (not (string/blank? name))]}
  (auth/verify [:write :map map-id]
    (let [content (if (:tempfile content)
                    (slurp (:tempfile content))
                    (str content))
          new-id (projects/save-network!
                  (:id auth/*current-user*)
                  project-id map-id name content)]
      
      (if (and run (restrictions/exceeded-jobs-per-week? auth/*current-user* project-id))
        (-> "Job limit exceeded"
            (response/response)
            (response/status 403)
            (cache-control/no-store))
        (do (when run
              (solver/queue-problem new-id (keyword run)))
            (-> (response/created (str new-id))
                (response/header "X-ID" new-id)))))))

(defn- network-geojson [{{:keys [net-id]} :params}]
  (auth/verify [:read :network net-id]
    (let [network  (projects/get-network net-id :include-content true)]
      (-> (:content network)
          (edn/read-string)
          (converter/network-problem->geojson)
          (json/encode)
          (response/response)
          (response/content-type "application/json")
          (attachment-disposition (str (:name network) ".json"))
          (cache-control/no-store)))))

(defn- delete-networks! [{{:keys [network-name map-id project-id]} :params :as request}]
  (auth/verify [:write :map map-id] ;; yes/no?
    (projects/delete-networks! map-id (url-decode network-name))
    (if (htmx-request? request)
      (maps-fragment project-id nil)
      deleted)))

(defn- rename-network-dialog [{{:keys [name map-id project-id]} :params}]
  (auth/verify [:write :map map-id]
    (-> (project-pages/rename-network-form project-id map-id name)
        (fragment)
        (cache-control/no-store))))

(defn- rename-networks! [{{:keys [from-name to-name map-id project-id]} :params}]
  {:pre [(string? from-name)
         (string? to-name)
         (not (string/blank? from-name))
         (not (string/blank? to-name))
         (int? map-id)]}
  (auth/verify [:write :map map-id]
    (projects/rename-networks! map-id from-name (string/trim to-name))
    (response/redirect (str "/project/" project-id) :see-other)))

(def map-routes
  [["/new" map-wizard/wizard-routes]
   [["/" [long :map-id]] {"" {:delete delete-map!}
                          "/delete" {:get delete-map-page :post delete-map!}
                          "/rename" {:get rename-map-dialog :post rename-map!}
                          "/icon.png" map-icon
                          "/data.json" map-geojson-data
                          ["/t/" [long :z] "/" [long :x] "/" [long :y]] map-vector-tile
                          ["/d/" [long :z] "/" [long :x] "/" [long :y] ".png"] map-heat-density-tile
                          ["/cd/" [long :z] "/" [long :x] "/" [long :y] ".png"] map-cold-density-tile
                          "/net" [["/new" {:get new-network-page
                                           :post network-save!}]
                                  ["/rename" {:get rename-network-dialog
                                               :post rename-networks!}]
                                  [["/" [long :net-id]]
                                   {"" {:get  network-editor-page
                                        :head network-poll-status
                                        :post network-save!}
                                    "/status" network-poll-status
                                    "/data.json" {:get network-geojson}
                                    }]
                                  [["/" [#".+" :network-name]] {:delete delete-networks!}]]}]])

(def project-routes
  ["/project"
   [["/" project-page]
    ["/new" {:get new-project-page :post create-project!}]
    [["/" [long :project-id]]

     (let [root {:get project-page
                 :delete delete-project!}]
       {""       root
        "/"      root
        "/maps"   project-maps
        "/rename" {:get rename-project-dialog :post rename-project!}
        "/leave"  {:get leave-project-dialog :post leave-project!}
        "/delete" {:delete delete-project!
                   :get    delete-project-page
                   :post   delete-project!}
        "/users"  {:get project-users-dialog :post set-project-users!}
        "/users/edit" {:post edit-project-users}
        "/lidar"  lidar-routes/lidar-routes
        "/map"    map-routes})
     ]
    ]
   ])


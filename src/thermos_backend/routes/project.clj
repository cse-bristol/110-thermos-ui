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
            [thermos-backend.config :refer [config]]
            [thermos-backend.importer.core :as importer]
            [clojure.java.io :as io]
            [ring.util.io :as ring-io]
            [cheshire.core :as json]
            [clojure.tools.logging :as log]
            [thermos-util.converter :as converter]
            [clojure.edn :as edn]
            [thermos-backend.solver.core :as solver])
  (:import [javax.mail.internet InternetAddress]
           [java.io ByteArrayInputStream]))

(defn- parse-email [text]
  (try
    (let [address (InternetAddress. text)]
     {:id (.getAddress address)
      :name (or (.getPersonal address)
                (.getAddress address))})
    (catch javax.mail.internet.AddressException e)))

(defn- parse-emails [text]
  (keep (comp parse-email string/trim)
        (string/split text #"[\n,]+")))

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
        (assoc :user (:id auth/*current-user*)))))

(defn- project-page [{{:keys [project-id]} :params :as req}]
  (auth/verify [:read :project project-id]
    (-> (project-pages/project-page (get-project-data project-id))
        (html)
        (cache-control/no-store))))

(defn- project-data-poll [{{:keys [project-id]} :params}]
  (auth/verify [:read :project project-id]
    (-> (get-project-data project-id)
        (transit-json)
        (cache-control/no-store))))

(defn- leave-project! [{{:keys [project-id]} :params}]
  (auth/verify [:leave :project project-id]
    (projects/leave! project-id (:id auth/*current-user*))
    (response/redirect ".")))

(defn- delete-project! [{{:keys [project-name project-id]} :params
                         method :request-method}]
  (auth/verify [:delete :project project-id]
    (cond (or (= :delete method)
              (and (= :post method)
                   (= (string/lower-case project-name)
                      (string/lower-case (:name (projects/get-project project-id))))))
          (do (projects/delete-project! project-id)
              deleted)
          
          (= :post method)
          (response/redirect "delete?wrong-name=1"))))

(defn- delete-project-page [{{:keys [project-id wrong-name]} :params}]
  (auth/verify [:delete :project project-id]
    (-> (project-pages/delete-project-page (projects/get-project project-id) wrong-name)
        (html)
        (cache-control/no-store))))

(defn- set-project-users! [{{:keys [users public project-id]} :params}]
  (auth/verify [:share :project project-id]
    (projects/set-users! project-id auth/*current-user* users)
    (projects/make-public! project-id (boolean public))
    (response/redirect ".")))

(defn- map-icon [{{:keys [map-id]} :params}]
  (auth/verify [:read :map map-id]
    (-> (maps/get-icon map-id)
        (ByteArrayInputStream.)
        (response/response)
        (response/content-type "image/png"))))

(defn- new-map-page [_] (html (map-pages/create-map-form)))
(defn- create-new-map! [{{:keys [project-id name description] :as params} :params}]
  (auth/verify [:modify :project project-id]
    (let [map-id (maps/create-map!
                  ;; why not store the args within the map object
                  project-id name description
                  params)]
      (importer/queue-import {:map-id map-id})
      (response/created (str "/project/" project-id)))))

(defn- upload-map-data [{{:keys [file project-id]} :params}]
  (auth/verify [:modify :project project-id]
    (let [files (if (vector? file) file [file])
          ident (str (java.util.UUID/randomUUID))
          target-dir (io/file (:import-directory config) ident)]
      
      (.mkdirs target-dir)

      (doseq [file files]
        (let [target-file ^java.io.File (io/file target-dir (:filename file))]
          (java.nio.file.Files/move
           (.toPath (:tempfile file))
           (.toPath target-file)
           (into-array java.nio.file.CopyOption []))))
      
      (response/response
       (assoc (importer/get-file-info target-dir) :id ident)))))

(defn- delete-map! [{method :request-method {:keys [map-id]} :params}]
  (auth/verify [:delete :map map-id]
    (when (#{:delete :post} method)
      (maps/delete-map! map-id)
      (cond (= :delete method) deleted
            (= :post method) (response/redirect "../../")))))

(defn- delete-map-page [{{:keys [map-id]} :params}]
  (auth/verify [:delete :map map-id]
    (html (map-pages/delete-map-page))))

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

(defn- new-network-page [{{:keys [mode map-id]} :params}]
  (auth/verify [:read :map map-id]
    (-> (editor/editor-page nil
                            nil
                            (if (= mode "cooling")
                              :cooling
                              :heating)
                            (maps/get-map-bounds map-id)
                            (nil? (projects/get-map-project-auth
                                   map-id
                                   (:id auth/*current-user*))))
        (response/response)
        (response/status 200)
        (response/content-type "text/html"))))

(defn- network-editor-page [{{:keys [net-id]} :params {accept "accept"} :headers}]
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
                                    (nil? project-auth))
                
                (response/response)
                (response/status 200)
                (response/content-type "text/html")))
          
          (cache-control/no-store)
          (response/header "X-Queue-Position" (:queue-position info))
          (response/header "X-Name" (:name info))
          (response/header "X-Run-State" (:state info))))))

(defn- network-poll-status [{{:keys [net-id]} :params}]
  (auth/verify [:read :network net-id]
    (let [info (projects/get-network net-id)]
      (-> (transit-json info)
          (response/status 200)
          (response/header "X-Name" (:name info))
          (response/header "X-Queue-Position" (:queue-position info))
          (response/header "X-Run-State" (:state info))
          (cache-control/no-store)))))

(defn- network-save! [{{:keys [name run content map-id project-id]} :params}]
  {:pre [(contains? #{nil "network" "supply" "both"} run)
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
      (when run
        (solver/queue-problem new-id (keyword run)))
      (-> (response/created (str new-id))
          (response/header "X-ID" new-id)))))

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

(defn- delete-networks! [{{:keys [network-name map-id]} :params}]
  (auth/verify [:write :map map-id] ;; yes/no?
    (projects/delete-networks! map-id (url-decode network-name))
    deleted))

(def map-routes
  [["/new" {:get new-map-page :post create-new-map!}]
   ["/new/add-file" {:post upload-map-data}]
   [["/" [long :map-id]] {"" {:delete delete-map!}
                          "/delete" {:get delete-map-page :post delete-map!}
                          "/icon.png" map-icon
                          "/data.json" map-geojson-data
                          ["/t/" [long :z] "/" [long :x] "/" [long :y]] map-vector-tile
                          ["/d/" [long :z] "/" [long :x] "/" [long :y] ".png"] map-heat-density-tile
                          ["/cd/" [long :z] "/" [long :x] "/" [long :y] ".png"] map-cold-density-tile
                          "/net" [["/new" {:get new-network-page
                                           :post network-save!}]
                                  [["/" [long :net-id]]
                                   {"" {:get  network-editor-page
                                        :head network-poll-status
                                        :post network-save!}
                                    
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
        "/poll.t" project-data-poll
        "/leave"  {:post leave-project!}
        "/delete" {:delete delete-project!
                   :get    delete-project-page
                   :post   delete-project!}
        "/users"  {:post set-project-users!}
        "/map"    map-routes})
     ]
    ]
   ])


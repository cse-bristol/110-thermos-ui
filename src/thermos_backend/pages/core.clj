(ns thermos-backend.pages.core
  (:require [compojure.core :refer :all]
            [compojure.coercions :refer [as-int]]
            [thermos-backend.auth :as auth]
            [thermos-backend.config :refer [config]]
            [thermos-backend.pages.landing-page :refer [landing-page]]
            [thermos-backend.pages.user-settings :refer [settings-page]]
            [thermos-backend.pages.projects :refer [new-project-page
                                                    project-page
                                                    delete-project-page]]
            [thermos-backend.pages.maps :as map-pages]
            [thermos-backend.pages.help :refer [help-page help-search]]
            [thermos-backend.pages.admin :as admin]
            [thermos-backend.pages.editor :refer [editor-page]]
            [ring.util.response :as response]
            [ring.util.io :as ring-io]

            [thermos-util.converter :refer [network-problem->geojson]]

            [thermos-backend.db.projects :as projects]
            [thermos-backend.db.maps :as maps]
            [thermos-backend.importer.core :as importer]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [thermos-backend.solver.core :as solver]
            [cognitect.transit :as transit]
            [thermos-backend.db.users :as users]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [thermos-backend.queue :as queue]
            [thermos-backend.pages.cache-control :as cache-control])
    
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
        (string/split text #"\n")))

(defn- as-double [x]
  (and x (try (Double/parseDouble x)
              (catch NumberFormatException e))))

(defn- streaming-map [map-id]
  (ring-io/piped-input-stream
   (fn [o]
     (with-open [w (io/writer o)]
       (doto w
         (.write "{ \"type\": \"FeatureCollection\", ")
         (.write "  \"features\": ["))

       (let [first (atom true)]
         (maps/stream-features
          map-id
          (fn [feature]
            (if @first (reset! first false) (.write w ","))

            (json/write
             {:type :Feature
              :geometry (json/read-str (:geometry feature))
              :properties (dissoc feature :geometry)}
             w)
            
            (.write w "\n"))
          ))

       (.write w "]}"))
     )))

(defn- handle-map-creation
  [project-id
   {map-name :name description :description
    :as params}]
  {:pre [(string? map-name)
         (string? description)
         (not (string/blank? map-name))]}
  (let [map-id (maps/create-map!
                ;; why not store the args within the map object
                project-id map-name description
                params)]
    (importer/queue-import {:map-id map-id})))

(defn- transit-json-response [data]
  (let [stream (java.io.ByteArrayOutputStream. 1024)
        w (transit/writer stream :json)]
    (transit/write w data)
    (-> (response/response (.toString stream))
        (response/content-type "application/transit+json; charset=utf-8"))))

(defn- attachment-disposition [r filename]
  (-> r (response/header "Content-Disposition"
                         (str "attachment"
                              (when filename
                                (str "; filename=\"" filename "\""))))))

(defn- get-project [id]
  (let [project (projects/get-project id)]
    (assoc project :user-is-admin
           (some #{:admin}
                 (->> (:users project)
                      (filter (comp #{(:id auth/*current-user*)} :id))
                      (map :auth))
                 ))))

(def deleted {:status 204})

(defroutes page-routes
  (GET "/favicon.ico" [] (response/resource-response "/public/favicon.ico"))

  (GET "/help" []
    (response/redirect "/help/index.html"))

  (GET "/help/search" [q]
    (help-search q))
  
  (GET "/help/:section" [section]
    (help-page section))

  (context "/admin" []
    (auth/restricted
     {:sysadmin true}
     
     (GET "/" []
       (admin/admin-page
        (users/users)
        (queue/list-tasks)))

     (GET "/send-email" []
       (admin/send-email-page))

     (POST "/send-email" [subject message]
       (users/send-system-message! subject message)
       (response/redirect "/admin"))
     
     (GET "/clean-queue/:queue-name" [queue-name purge]
       (if purge
         (queue/erase (keyword queue-name))
         (queue/clean-up (keyword queue-name)))
       (response/redirect "/admin"))

     (wrap-json-response
      (GET "/map-bounds" []
        (response/response (maps/get-map-bounds-as-geojson))))))
    
  (auth/restricted
   {:logged-in true}
   (GET "/" []
     (-> (landing-page
          (:name auth/*current-user*)
          (projects/user-projects (:id auth/*current-user*)))
         (response/response)
         (response/content-type "text/html")
         (cache-control/no-store)))
   
   (GET "/settings" []
     (-> (settings-page auth/*current-user*)
         (response/response)
         (response/content-type "text/html")
         (cache-control/no-store)))

   (POST "/settings" [new-name password-1 password-2
                      system-messages]
     (users/update-user!
      (:id auth/*current-user*)
      new-name
      (and (= password-1 password-2) password-1)
      (boolean system-messages))
     
     (response/redirect "."))
   
   (context "/project" []
     (GET "/new" [] (new-project-page))
     
     (POST "/new" [name description members]
       (let [members (parse-emails members)
             new-project-id (projects/create-project!
                             auth/*current-user*
                             name description members)]
         (response/redirect (str "./" new-project-id))))

     (auth/restricted-context "/:project-id" [project-id :<< as-int]
       {:project project-id}
       (GET "/" []
         (-> (project-page
              (get-project project-id))
             (response/response)
             (response/content-type "text/html")
             (cache-control/no-store)))

       (GET "/poll.t" []
         (-> (transit-json-response
              (get-project project-id))
             (cache-control/no-store)))

       (auth/restricted
         {:project-admin project-id}

         (DELETE "/" []
           (projects/delete-project! project-id)
           deleted)

         (GET "/delete" [wrong-name]
           (-> (delete-project-page (projects/get-project project-id) wrong-name)
               (response/response)
               (response/content-type "text/html")
               (cache-control/no-store)))
         
         (POST "/delete" [project-name]
           (if (= (string/lower-case project-name)
                  (string/lower-case (:name (projects/get-project project-id))))
             (do (projects/delete-project! project-id)
                 (response/redirect "/"))
             (response/redirect "delete?wrong-name=1")))

         (POST "/users" [users :as r]
           (projects/set-users! project-id auth/*current-user* users)
           (response/redirect ".")))
       
       (context "/map" []
         (GET "/new" []
           (map-pages/create-map-form))

         (POST "/new" [& params]
           ;; body params contains the form state.
           (handle-map-creation project-id params)
           (response/created (str "/project/" project-id)))

         (POST "/new/add-file" [file :as {s :session}]
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
              (assoc (importer/get-file-info target-dir)
                     :id ident))))
         
         (auth/restricted-context "/:map-id" [map-id :<< as-int]
           {:map map-id}
           
           (auth/restricted
             {:project-admin project-id}
             (GET "/delete" [] (map-pages/delete-map-page))

             (DELETE "/" []
               (maps/delete-map! map-id)
               deleted)
             
             (POST "/delete" []
               (maps/delete-map! map-id)
               (response/redirect "../..")))
           
           (wrap-json-response
            (GET "/t/:z/:x/:y" [z :<< as-int x :<< as-int y :<< as-int]
              (response/response (maps/get-tile map-id z x y))))
           
           (GET "/d/:z/:x/:y.png" [z :<< as-int x :<< as-int y :<< as-int]
             (-> (maps/get-density-tile map-id z x y)
                 (ByteArrayInputStream.)
                 (response/response)
                 (response/content-type "image/png")))
           
           (GET "/icon.png" []
             (-> (maps/get-icon map-id)
                 (ByteArrayInputStream.)
                 (response/response)
                 (response/content-type "image/png")))

           (GET "/data.json" []
             (-> (response/response (streaming-map map-id))
                 (response/content-type "application/json")
                 (attachment-disposition
                  (str (:name (maps/get-map map-id)) ".json"))))

           (DELETE "/net/:network-name" [network-name]
             ;; delete all networks in project with name
             (projects/delete-networks! map-id network-name)
             deleted)
           
           (auth/restricted-context "/net/:net-id" [net-id]
             (when-let [id (as-int net-id)] {:network id})
             (GET "/" {{accept "accept"} :headers}
               (let [accept (set (string/split accept #","))
                     info (when-not (= "new" net-id)
                            (projects/get-network (as-int net-id) :include-content true))]
                 (-> (cond
                       (accept "application/edn")
                       (-> (response/response (:content info))
                           (response/status 200)
                           (response/content-type "text/edn"))
                       
                       (or (accept "*/*")
                           (accept "text/html"))
                       (-> (editor-page (:name info)
                                        (:content info)
                                        (when-not info
                                          (maps/get-map-bounds map-id)))
                           
                           (response/response)
                           (response/status 200)
                           (response/content-type "text/html")))
                     
                     (cache-control/no-store)
                     (response/header "X-Queue-Position" (:queue-position info))
                     (response/header "X-Name" (:name info))
                     (response/header "X-Run-State" (:state info)))))

             (GET "/data.json" []
               ;; convert the problem into geojson for external save
               (let [network  (projects/get-network (as-int net-id) :include-content true)]
                 (-> (:content network)
                     (edn/read-string)
                     (network-problem->geojson)
                     (json/write-str)
                     (response/response)
                     (response/content-type "application/json")
                     (attachment-disposition (str (:name network) ".json"))
                     (cache-control/no-store))))

             (HEAD "/" []
               (let [info (projects/get-network net-id)]
                 (-> (response/status 200)
                     (response/header "X-Name" (:name info))
                     (response/header "X-Queue-Position" (:queue-position info))
                     (response/header "X-Run-State" (:state info))
                     (cache-control/no-store))))
             
             (POST "/" [name run content]
               (let [content (if (:tempfile content)
                               (slurp (:tempfile content))
                               (str content))
                     new-id (projects/save-network!
                             (:id auth/*current-user*)
                             project-id map-id name content)]
                 (when run (solver/queue-problem new-id))
                 (-> (response/created (str new-id))
                     (response/header "X-ID" new-id))))))
         
         )
       ))))

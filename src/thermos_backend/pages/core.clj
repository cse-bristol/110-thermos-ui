(ns thermos-backend.pages.core
  (:require [compojure.core :refer [defroutes routes context]]
            [compojure.coercions :refer [as-int]]
            [thermos-backend.auth :as auth :refer [GET POST DELETE HEAD]]
            [thermos-backend.pages.landing-page :refer [landing-page]]
            [thermos-backend.pages.projects :refer [new-project-page project-page]]
            [thermos-backend.pages.maps :refer [create-map-form]]
            [thermos-backend.pages.editor :refer [editor-page]]
            [ring.util.response :as response]

            [thermos-backend.db.projects :as projects]
            [thermos-backend.db.maps :as maps]
            [thermos-backend.importer.core :as importer]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [thermos-backend.solver.core :as solver])
  
  (:import [javax.mail.internet InternetAddress]
           [java.io ByteArrayInputStream]))

(defn- parse-email [text]
  (try
    (let [address (InternetAddress. text)]
     {:email (.getAddress address)
      :name (or (.getPersonal address)
                (.getAddress address))})
    (catch javax.mail.internet.AddressException e)))

(defn- parse-emails [text]
  (keep (comp parse-email string/trim)
        (string/split text #"\n")))

(defn- as-double [x]
  (and x (try (Double/parseDouble x)
              (catch NumberFormatException e))))

(defn- handle-map-import [map-id request]
  (let [params (:params request)
        {:keys [building-source
                building-gis-files
                
                road-source
                road-gis-files

                degree-days

                osm-area
                
                use-lidar

                use-benchmarks
                benchmarks-file
                
                road-field-map
                building-field-map]} params

        ensure-vector #(if (vector? %) % [%])
        
        building-gis-files (ensure-vector building-gis-files)
        road-gis-files (ensure-vector road-gis-files)
        benchmarks-file (ensure-vector benchmarks-file)

        degree-days (as-double degree-days)

        use-lidar (boolean use-lidar)
        use-benchmarks (boolean use-benchmarks)
        
        road-field-map (edn/read-string road-field-map)
        building-field-map (edn/read-string building-field-map)]

    (importer/queue-import
     (cond-> {:osm-area osm-area
              :degree-days degree-days
              :map-id map-id}
       use-benchmarks    (cond-> (seq benchmarks-file)
                           (assoc :benchmarks-file benchmarks-file)
                           :else (assoc :default-benchmarks true))
       use-lidar         (assoc :use-lidar true)

       (not= building-source "openstreetmap")
       (assoc :buildings-file building-gis-files
              :buildings-field-map building-field-map)
       
       (not= road-source "openstreetmap")
       (assoc :roads-file road-gis-files
              :roads-field-map road-field-map)))))
  
(defroutes page-routes
  (GET "/favicon.ico" [] (response/resource-response "/public/favicon.ico"))

  (auth/with-restrict {:public false}
    (GET "/" []
      (landing-page (:name auth/*current-user*)
                    ;; this second is no use
                    ;; we want something better
                    (projects/user-projects (:id auth/*current-user*))))

    (context "/project" []
      (GET "/new" [] (new-project-page))
      (POST "/new" [name description members]
        (let [u auth/*current-user*
              members (parse-emails members)
              members (conj members {:email (:id u) :name (:name u) :auth :admin})
              new-project-id (projects/create-project! name description members)]
          (response/redirect (str "./" new-project-id))))

      (context "/:project-id" [project-id :<< as-int]
        (auth/with-restrict {:project project-id}
          (GET "/" [] (project-page (projects/get-project project-id)))
          (context "/map" []
            (GET "/new" [] (create-map-form))
            (POST "/new" [map-name map-description :as request]
              (when-let [map-id (maps/create-map! project-id map-name map-description)]
                (handle-map-import map-id request)
                (response/redirect "../")))
            (context "/:map-id" [map-id :<< as-int]
              (wrap-json-response
               (GET "/t/:z/:x/:y" [z :<< as-int x :<< as-int y :<< as-int]
                 ;; this is a bit ugly and makes for bad caching as well
                 ;; maybe it would be better to load the map id into the map
                 ;; we could hit the database here for map->network, or we
                 ;; could ask maps/get-tile to do it
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
              
              (context "/net/:net-id" [net-id]
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
                                             (maps/get-map-centre map-id)))
                              (response/response)
                              (response/status 200)
                              (response/content-type "text/html")))

                        (response/header "X-Queue-Position" (:queue-position info))
                        (response/header "X-Name" (:name info))
                        (response/header "X-Run-State" (:state info)))))

                (HEAD "/" []
                  (let [info (projects/get-network net-id)]
                    (-> (response/status 200)
                        (response/header "X-Name" (:name info))
                        (response/header "X-Queue-Position" (:queue-position info))
                        (response/header "X-Run-State" (:state info)))))
                
                (POST "/" [name run content]
                  (let [content (if (:tempfile content)
                                  (slurp (:tempfile content))
                                  (str content))
                        new-id (projects/save-network! project-id map-id name content)]
                    (when run (solver/queue-problem new-id))
                    (-> (response/created (str new-id))
                        (response/header "X-ID" new-id)))))
              
              ))
          )))

    ))



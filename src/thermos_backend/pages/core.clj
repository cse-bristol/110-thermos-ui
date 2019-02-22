(ns thermos-backend.pages.core
  (:require [compojure.core :refer :all]
            [compojure.coercions :refer [as-int]]
            [thermos-backend.auth :as auth]
            [thermos-backend.pages.landing-page :refer [landing-page]]
            [thermos-backend.pages.projects :refer [new-project-page project-page]]
            [ring.util.response :as response]

            [thermos-backend.db.projects :as projects]
            [clojure.string :as string])
  (:import [javax.mail.internet InternetAddress]))

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

(defroutes page-routes
  (auth/with-restrict {}
    (GET "/" []
      (landing-page (:name auth/*current-user*)
                    ;; this second is no use
                    ;; we want something better
                    (projects/user-projects (:id auth/*current-user*))))

    (GET "/project/new" []
      (new-project-page))

    (POST "/project/new" [name description members]
        (let [u auth/*current-user*
              members (parse-emails members)
              members (conj members {:email (:id u) :name (:name u) :auth :admin})
              new-project-id (projects/create-project! name description members)]
          (response/redirect (str "./" new-project-id))))
    
    (context "/project/:id" [id :<< as-int]
      (auth/with-restrict {:project id}
        (GET "/" []
            (project-page
             (projects/get-project id)))

        (DELETE "/" [])

        (context "/map" []
          (GET "/new" [] "new map + import")
          (GET "/:id" [id :<< as-int] "manage map (extend/replace values)")
          (DELETE "/:id" [id :<< as-int] "delete map")
          (GET "/:id/data" [id :<< as-int] "download map data")
          
          )
        )
      
      )
    ))


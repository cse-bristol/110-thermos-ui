;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.routes.landing
  (:require [ring.util.response :as response]
            [thermos-backend.routes.responses :refer :all]
            [thermos-backend.auth :as auth]
            [thermos-backend.pages.cache-control :as cache-control]
            [thermos-backend.pages.landing-page :as landing-page]
            [thermos-backend.pages.user-settings :as user-settings]
            [compojure.coercions :refer [as-int]]
            [thermos-backend.db.users :as users]
            [thermos-backend.db.projects :as projects]))

(defn- landing-page [{{changes :changes} :params}]
  (auth/verify :logged-in
    (let [changes (as-int changes)]
      (when changes
        (users/seen-changes! (:id auth/*current-user*) changes))
      
      (-> (landing-page/landing-page
           (cond-> auth/*current-user*
             changes (assoc :changelog-seen changes))
           (projects/user-projects (:id auth/*current-user*)))
          (html)
          (cache-control/no-store)))))

(defn- settings-page [_]
  (auth/verify :logged-in
    (-> (user-settings/settings-page auth/*current-user*)
        (html)
        (cache-control/no-store))))

(defn- update-settings! [{{new-name :new-name
                           password-1 :password-1
                           password-2 :password-2
                           system-messages :system-messages}
                          :params}]
  (auth/verify :logged-in
    (users/update-user!
     (:id auth/*current-user*)
     new-name
     (and (= password-1 password-2) password-1)
     (boolean system-messages))
    (response/redirect ".")))

(def landing-routes
  [""
   [["/" landing-page]
    ["/settings" {:get settings-page
                  :post update-settings!}]]])

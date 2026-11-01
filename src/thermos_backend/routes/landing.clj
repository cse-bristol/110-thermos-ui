;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.routes.landing
  (:require [ring.util.response :as response]
            [clojure.string :as string]
            [thermos-backend.routes.responses :refer :all]
            [thermos-backend.auth :as auth]
            [thermos-backend.pages.cache-control :as cache-control]
            [thermos-backend.pages.landing-page :as landing-page]
            [thermos-backend.pages.user-settings :as user-settings]
            [compojure.coercions :refer [as-int]]
            [thermos-backend.db.users :as users]
            [thermos-backend.passwords :as passwords]
            [thermos-backend.notifications :as notifications]
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

(defn- settings-page [{{problem :problem} :params session :session}]
  (auth/verify :logged-in
    (-> (user-settings/settings-page
         auth/*current-user*
         {:must-change-password (boolean (::auth/must-change-password session))
          :problem (#{"mismatch" "weak"} problem)})
        (html)
        (cache-control/no-store))))

(defn- update-settings! [{{new-name :new-name
                           password-1 :password-1
                           password-2 :password-2
                           system-messages :system-messages
                           job-emails :job-emails}
                          :params
                          :as request}]
  (auth/verify :logged-in
    (let [new-password (not (and (string/blank? password-1)
                                 (string/blank? password-2)))
          must-change  (::auth/must-change-password (:session request))
          problem      (cond
                         (not= password-1 password-2)              "mismatch"
                         (and must-change (not new-password))      "weak"
                         (and new-password
                              (passwords/password-problem password-1)) "weak")]
      (if problem
        (response/redirect (str "/settings?problem=" problem))
        (do (users/update-user!
             (:id auth/*current-user*)
             new-name
             (when new-password password-1)
             (boolean system-messages)
             (boolean job-emails))
            (cond->> (response/redirect ".")
              new-password (auth/password-changed request)))))))

(defn- push-key [_]
  (auth/verify :logged-in
    (-> (json {:public-key (notifications/public-key)})
        (cache-control/no-store))))

;; these take JSON, which a form on another site can't send us
(defn- push-subscribe! [{{:keys [endpoint p256dh auth]} :body-params}]
  (auth/verify :logged-in
    (if (and (string? endpoint) (string/starts-with? endpoint "https://")
             (string? p256dh) (string? auth))
      (do (notifications/subscribe! (:id auth/*current-user*)
                                    {:endpoint endpoint :p256dh p256dh :auth auth})
          (cache-control/no-store deleted))
      (-> (response/bad-request "Bad subscription")
          (cache-control/no-store)))))

(defn- push-unsubscribe! [{{:keys [endpoint]} :body-params}]
  (auth/verify :logged-in
    (when (string? endpoint)
      (notifications/unsubscribe! (:id auth/*current-user*) endpoint))
    (cache-control/no-store deleted)))

(defn- push-test! [_]
  (auth/verify :logged-in
    (-> (json {:sent (notifications/push!
                      (:id auth/*current-user*)
                      {:title "THERMOS notifications are working"
                       :body "You'll get a notification like this when a model run or map import finishes."
                       :url "/settings"
                       :tag "test"})})
        (cache-control/no-store))))

(def landing-routes
  [""
   [["/" landing-page]
    ["/settings/push" {:get push-key
                       :post push-subscribe!
                       :delete push-unsubscribe!}]
    ["/settings/push/test" {:post push-test!}]
    ["/settings" {:get settings-page
                  :post update-settings!}]]])

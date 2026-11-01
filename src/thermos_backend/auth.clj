;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.auth
  "The web application's authentication is quite simple, and most of it is in here.
  In a route you should be able to say (auth/restrict {:user (some-user)} ...).
  "
  (:require [honeysql.helpers :as sql]
            [ring.util.response :as response]
            [buddy.hashers :as hash]
            [compojure.core :refer :all]
            [clojure.string :as string]
            [thermos-backend.pages.login-form :refer [login-form]]
            [thermos-backend.current-uri :refer [*current-uri*]]
            [clojure.tools.logging :as log]
            [thermos-backend.db.users :as users]
            [thermos-backend.db.projects :as projects]
            [thermos-backend.login-throttle :as throttle]
            [thermos-backend.passwords :as passwords]
            [thermos-backend.pages.cache-control :as cache-control]))

(def ^:dynamic *current-user* nil)

(defn redirect-to-login-page []
  (-> (response/redirect (str "/login?redirect-to=" *current-uri*))
      (cache-control/no-store)))

(def forbidden (-> (response/response
                    "I can neither confirm nor deny that this exists.
If it does exist, you don't have privileges to see it.")
                   (response/content-type "text/plain")
                   (response/status 401)
                   (cache-control/no-store)))

(defn- current-sysadmin? []
  (= :admin (:auth *current-user*)))

(defn forbidden-or-login []
  (if *current-user* forbidden (redirect-to-login-page)))

(defmulti verify* (fn [t] (cond
                            (keyword? t) t
                            (vector? t) (second t))))

(defmethod verify* :logged-in [_]
  (when-not *current-user* (redirect-to-login-page)))

(defmethod verify* :sysadmin [_]
  (when-not (current-sysadmin?) (forbidden-or-login)))

(defmethod verify* :project [[operation _ project-id]]
  (let [user-project-auth (-> *current-user* :projects (get project-id) :auth)
        am-member         (not (nil? user-project-auth))
        am-sysadmin       (current-sysadmin?)
        am-project-admin  (= :admin user-project-auth)]
    (when-not (case operation
                :read    (or am-sysadmin am-member (projects/is-public-project? project-id))
                :share   (or am-sysadmin am-project-admin)
                :delete  (or am-sysadmin am-project-admin)
                :rename  (or am-sysadmin am-project-admin)
                :modify  (or am-sysadmin am-member)
                :leave   am-member
                
                (log/warn "Unknown project operation" operation))
      (forbidden-or-login))))

(defmethod verify* :map [[operation _ map-id]]
  (let [am-sysadmin (current-sysadmin?)
        am-member   (contains? (:maps *current-user*) map-id)]
    (when-not (case operation
                :delete  (or am-sysadmin am-member)
                :read    (or am-sysadmin am-member (projects/is-public-map? map-id))
                :write   (or am-sysadmin am-member)
                
                (log/warn "Unknown map operation" operation))
      
      (forbidden-or-login))))

(defmethod verify* :network [[operation _ net-id]]
  (let [am-sysadmin (current-sysadmin?)
        am-member   (contains? (:networks *current-user*) net-id)]
    (when-not (case operation
                :read    (or am-sysadmin am-member (projects/is-public-network? net-id))
                (log/warn "Unknown network operation" operation))
      (forbidden-or-login))))

(defmethod verify* :job [[operation _ job-id]]
  (let [am-sysadmin (current-sysadmin?)]
    (when-not (and
               (or (= operation :restart) (= operation :cancel))
               (or am-sysadmin
                   (contains? (:jobs *current-user*) job-id)))
      (forbidden))))

(defmethod verify* :default [query]
  (log/warn "Unknown type of permission" query)
  (forbidden-or-login))

(defmacro verify {:style/indent :defn}
  [rules & body]
  `(let [resp# (verify* ~rules)]
     (if resp#
       (do
         (when *current-user*
           (log/warn "Invalid access by" (:id *current-user*) ~rules))
         resp#)
       (do ~@body))))

(def ^:private allowed-before-password-change
  "Where someone who must change their password can go"
  #{"/settings" "/logout"})

(defn wrap-auth
  "Ring middleware that gets the ::user-id out of the session
  and uses it to set `*current-user*` for the rest of the request.

  This done, you can use `restrict` to control access within handlers
  or `with-restrict` to restrict routes (the middleware version of
  restrict)

  Someone who logged in with a password which is no longer good enough
  is kept on the settings page until they change it.
  "
  [h]
  (fn [r]
    (let [user-id (::user-id (:session r))
          user (and user-id (users/user-rights user-id))]
      (binding [*current-user* user]
        (if (and user
                 (::must-change-password (:session r))
                 (not (allowed-before-password-change (:uri r))))
          (-> (response/redirect "/settings")
              (cache-control/no-store))
          (h r))))))

;; (defn restricted* [handler requirement]
;;   (fn [request]
;;     (if (authorized requirement)
;;       (handler request)
;;       (-> (if *current-user*
;;             (-> (response/response
;;                  "I can neither confirm nor deny that this exists.
;; If it does exist, you don't have privileges to see it.")
;;                 (response/content-type "text/plain")
;;                 (response/status 401))
            
;;             (response/redirect (str "/login?redirect-to=" *current-uri*)))
;;           (response/header "Cache-Control" "no-store")))))


(defn handle-login [email password redirect-to]
  (let [email (string/lower-case email)
        wait  (throttle/seconds-to-wait email)]
    (cond
      (pos? wait)
      (do (log/info email "login refused for" wait "seconds")
          (response/redirect (str "/login?flash=wait&seconds=" wait)))

      (users/correct-password? email password)
      (do (log/info email "logged in")
          (throttle/succeeded! email)
          (users/logged-in! email)
          (if (passwords/password-problem password)
            (-> (response/redirect "/settings")
                (assoc :session {::user-id email
                                 ::must-change-password (or redirect-to "/")}))
            (-> (response/redirect (or redirect-to "/"))
                (assoc :session {::user-id email}))))

      :else
      (do (log/info email "login failed!")
          (throttle/failed! email)
          (response/redirect "/login?flash=failed")))))

(defn password-changed
  "Update `response` to a request from someone who has just changed their
  password. If they were made to, they can now go where they were going."
  [request response]
  (if-let [target (::must-change-password (:session request))]
    (-> response
        (assoc-in [:headers "Location"] target)
        (assoc :session (dissoc (:session request) ::must-change-password)))
    response))

(defn handle-logout []
  (log/info (:id *current-user*) "logged out")
  (-> (response/redirect "/")
      (update :session dissoc ::user-id)))

(defroutes auth-routes
  (GET "/login" [target flash seconds]
    (if *current-user*
      (response/redirect target)
      (login-form target flash seconds)))

  (GET "/token/:token" [token]
    ;; since the token is sent by email, knowing it is as good as a password.
    ;; we want to go to the user settings page when we handle it.
    (when-let [user-id (users/verify-reset-token token)]
      (-> (response/redirect "/settings")
          (update :session assoc ::user-id user-id))))
    
  (POST "/login" [username password redirect-to
                  create login forgot]
    (cond
      create
      (cond
        (passwords/password-problem password)
        (response/redirect "/login?flash=weak")

        (users/create-user! username username password)
        (handle-login username password "/")

        :else
        (response/redirect "/login?flash=exists"))

      login
      (handle-login username password redirect-to)

      forgot
      (do (users/emit-reset-token! username)
          (response/redirect "/login?flash=check-mail"))))

  (GET "/logout" []
    (handle-logout)))


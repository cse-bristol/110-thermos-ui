(ns thermos-backend.auth
  "The web application's authentication is quite simple, and most of it is in here.
  In a route you should be able to say (auth/restrict {:user (some-user)} ...).
  "
  (:require [thermos-backend.db.users :as db]
            [honeysql.helpers :as sql]
            [ring.util.response :as response]
            [buddy.hashers :as hash]
            [compojure.core :refer :all]
            [clojure.string :as string]
            [thermos-backend.pages.login-form :refer [login-form]]
            [thermos-backend.current-uri :refer [*current-uri*]]
            [clojure.tools.logging :as log]))

(def ^:dynamic *current-user* nil)

(defn authorized
  "Determine whether `this-user` is authorized according to the requirements:
  `user` is `this-user`'s user id
  `user` has access to `project`
  `user` is admin on `project-admin`
  `user` is `sysadmin`"

  ([{logged-in :logged-in
     user :user
     project :project
     project-admin :project-admin
     sysadmin :sysadmin :as restrict}]
   (authorized restrict *current-user*))
  
  ([{logged-in :logged-in
     user :user
     project :project
     project-admin :project-admin
     sysadmin :sysadmin
     :as restrict}
    this-user]
   
   (let [result (and (or (not logged-in) this-user)
                     (or (not user)
                         (= user (:id this-user)))
                     (or (not project)
                         (contains? (:projects this-user) project))
                     (or (not project-admin)
                         (= :admin (get-in this-user [:projects project-admin :auth] )))
                     (or (not sysadmin)
                         (= :admin (:auth this-user))))]
     (when-not result
       (log/warn "Authorization failure" this-user restrict)
       )
     result)))


(defn- do? [stuff]
  (if (seq (rest stuff))
    (cons 'do stuff)
    (first stuff)))

(defn wrap-auth
  "Ring middleware that gets the ::user-id out of the session
  and uses it to set `*current-user*` for the rest of the request.

  This done, you can use `restrict` to control access within handlers
  or `with-restrict` to restrict routes (the middleware version of
  restrict)
  "
  [h]
  (fn [r]
    (let [user-id (::user-id (:session r))
          user (and user-id (db/user-rights user-id))]
      (binding [*current-user* user]
        (h r)))))

(defn restricted* [handler requirement]
  (fn [request]
    (if (authorized requirement)
      (handler request)
      (-> (if *current-user*
            (-> (response/response "Unauthorized")
                (response/status 401))

            (response/redirect (str "/login?redirect-to=" *current-uri*)))
          (response/header "Cache-Control" "no-store")))))

(defmacro restricted [requirement & inner]
  (let [inner (if (seq (rest inner))
                (cons 'routes inner)
                (first inner))]
    `(wrap-routes
      ~inner
      restricted*
      ~requirement)))

(defn handle-login [email password redirect-to]
  (let [email (string/lower-case email)]
    (if (db/correct-password? email password)
      (do (log/info email "logged in")
          (-> (response/redirect (or redirect-to "/"))
              (update :session assoc ::user-id email)))
      (log/info email "login failed!"))))

(defn handle-logout []
  (-> (response/redirect "/")
      (update :session dissoc ::user-id)))

(defroutes auth-routes
  (GET "/login" [target flash]
    (login-form target flash))

  (GET "/token/:token" [token]
    ;; since the token is sent by email, knowing it is as good as a password.
    ;; we want to go to the user settings page when we handle it.
    (when-let [user-id (db/verify-reset-token token)]
      (-> (response/redirect "/settings")
          (update :session assoc ::user-id user-id))))
    
  (POST "/login" [username password redirect-to
                  create login forgot]
    (cond
      create
      (if (db/create-user! username username password)
        (handle-login username password "/")
        (response/redirect "/login?flash=exists"))

      login
      (handle-login username password redirect-to)

      forgot
      (do (db/emit-reset-token! username)
          (response/redirect "/login?flash=check-mail")))
    
    )

  (GET "/logout" []
    (handle-logout)))

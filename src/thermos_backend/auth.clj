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
            [clojure.tools.logging :as log]))

(def ^:dynamic *current-user* nil)

(defn authorized
  "Determine whether `this-user` is authorized according to the requirements:
  `user` is `this-user`'s user id
  `user` has access to `project`
  `user` is admin on `project-admin`
  `user` is `sysadmin`"
  [{user :user
    project :project
    project-admin :project-admin
    sysadmin :sysadmin}
   this-user]
  (and this-user
       (or (not user)
           (= user (:id this-user)))
       (or (not project)
           (contains? (:projects this-user) project))
       (or (not project-admin)
           (= :admin (get-in this-user [:projects project :auth] )))
       (or (not sysadmin)
           (= :admin (:auth this-user)))))

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

(defmacro restrict [to & body]
  `(cond
     (not *current-user*) ;; some login required
     (response/redirect "/login") ;; 401, render page here? is that mad? redirect?
     
     (not (authorized ~to *current-user*))
     {:status 401 :body "Forbidden"} ;; 401 straight error

     :else (do ~@body)))

(defmacro with-restrict [to & h]
  `(let [h# (routes ~@h)]
     ;; we don't want to reevalute h on every request
     ;; but we do need to reevalute to on every request
     ;; in case it is e.g. a variable we are closing over
     (fn [request#] (restrict ~to (h# request#)))))

(defn handle-login [email password redirect-to]
  (let [email (string/lower-case email)]
    (if (db/correct-password? email password)
      (do (log/info email "logged in")
          (-> (response/redirect "/")
              (update :session assoc ::user-id email)))
      (log/info email "login failed!")
      )))

(defn handle-logout []
  (-> (response/redirect "/")
      (update :session dissoc ::user-id)))

(defroutes auth-routes
  (GET "/login" [target flash]
    (login-form target flash))
  
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
      1
      ;; deal with forgot token
      
      )
    )

  (GET "/logout" []
       (handle-logout)))

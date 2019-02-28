(ns thermos-backend.auth
  "The web application's authentication is quite simple, and most of it is in here.
  In a route you should be able to say (auth/restrict {:user (some-user)} ...).
  "
  (:require [thermos-backend.db.users :as db]
            [honeysql.helpers :as sql]
            [ring.util.response :as response]
            [buddy.hashers :as hash]
            [compojure.core :as compojure]
            [clojure.string :as string]
            [thermos-backend.pages.login-form :refer [login-form]]
            [thermos-backend.current-uri :refer [*current-uri*]]
            [clojure.tools.logging :as log]))

(def ^:dynamic *current-user* nil)
(def ^:dynamic *current-restriction* {:public true})

(defn authorized
  "Determine whether `this-user` is authorized according to the requirements:
  `user` is `this-user`'s user id
  `user` has access to `project`
  `user` is admin on `project-admin`
  `user` is `sysadmin`"
  [{public :public
    user :user
    project :project
    project-admin :project-admin
    sysadmin :sysadmin}
   this-user]
  (and (or public this-user)
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
  `(if (not (authorized ~to *current-user*))
     (-> 
      (if *current-user*
        (-> (response/response "Unauthorized")
            (response/status 401))

        (response/redirect (str "/login?redirect-to=" *current-uri*)))
      
      (response/header "Cache-Control" "no-store"))

     (do ~@body)))

(defmacro with-restrict [to & stuff]
  `(let [handler# (compojure/routes ~@stuff)]
     (fn [request#]
       (binding [*current-restriction* ~to]
         (handler# request#)))))

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

(compojure/defroutes auth-routes
  (compojure/GET "/login" [target flash]
    (login-form target flash))
  
  (compojure/POST "/login" [username password redirect-to
                            create login forgot]
    (cond
      create
      (if (db/create-user! username username password)
        (handle-login username password "/")
        (response/redirect "/login?flash=exists"))

      login
      (handle-login username password redirect-to)

      forgot
      "not implemented yet"
      )
    )

  (compojure/GET "/logout" []
       (handle-logout)))

(defmacro GET [path args & body]
  `(compojure/GET ~path ~args
     (restrict *current-restriction* ~@body)))

(defmacro HEAD [path args & body]
  `(compojure/HEAD ~path ~args
     (restrict *current-restriction* ~@body)))

(defmacro POST [path args & body]
  `(compojure/POST ~path ~args
     (restrict *current-restriction* ~@body)))

(defmacro DELETE [path args & body]
  `(compojure/DELETE ~path ~args
     (restrict *current-restriction* ~@body)))

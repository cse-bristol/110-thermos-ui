(ns thermos-backend.routes.admin
  (:require [thermos-backend.pages.admin :as admin]
            [thermos-backend.db.users :as users]
            [thermos-backend.queue :as queue]
            [thermos-backend.routes.responses :refer :all]
            [thermos-backend.pages.cache-control :as cache-control]
            [ring.util.response :as response]
            [thermos-backend.auth :as auth]
            [thermos-backend.db.maps :as maps]))

(defn- admin-page [_]
  (auth/verify :sysadmin
    (-> (admin/admin-page
         (users/users)
         (queue/list-tasks))
        (html)
        (cache-control/no-store))))

(defn- view-job [{{:keys [job-id]} :params}]
  (auth/verify :sysadmin
    (-> (queue/job-details job-id)
        (admin/job-page)
        (html)
        (cache-control/no-store))))

(defn- act-on-job! [{{:keys [action job-id]} :params}]
  {:pre [(int job-id) (#{"restart" "cancel"} action)]}
  (auth/verify [(keyword action) :job job-id]
    (case action
      "restart" (queue/restart job-id)
      "cancel"  (queue/cancel job-id))
    (response/redirect (str job-id))))

(defn- send-email-page [_]
  (auth/verify :sysadmin
    (-> (admin/send-email-page)
        (html)
        (cache-control/no-store))))

(defn- send-email! [{{subject :subject message :message} :params}]
  (auth/verify :sysadmin
    (users/send-system-message! subject message)
    (response/redirect "/admin")))

(defn- clean-queue! [{{queue-name :queue-name purge :purge} :params}]
  (auth/verify :sysadmin
    (if purge
      (queue/erase (keyword queue-name))
      (queue/clean-up (keyword queue-name)))
    (response/redirect "/admin")))

(defn- get-map-bounds [_]
  (auth/verify :sysadmin
    (json (maps/get-map-bounds-as-geojson))))

(def admin-routes
  ["/admin"
   {""            admin-page
    "/send-email" {:get send-email-page :post send-email!}
    ["/job/" [long :job-id]] {:get view-job :post act-on-job!}
    ["/clean-queue/" :queue-name] clean-queue!
    "/map-bounds" get-map-bounds}])


(ns thermos-frontend.pages.admin
  (:require [ajax.core :refer [POST]]))

(enable-console-print!)

(def updates (atom {}))

(defn ^:export set-user-auth [user-id auth]
  (swap! updates assoc-in [user-id :auth] auth))

(defn ^:export mark-for-deletion [user-id delete?]
  (swap! updates assoc-in [user-id :delete] delete?))

(defn ^:export submit-updates []
  (POST "update-users"
    {:format :json
     :params (clj->js @updates)
     :handler
     (fn [_] (js/window.location.replace "/admin"))}))

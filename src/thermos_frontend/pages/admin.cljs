;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

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

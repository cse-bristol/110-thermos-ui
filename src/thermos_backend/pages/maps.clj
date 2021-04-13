;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.pages.maps
  (:require [thermos-backend.pages.common :refer [page anti-forgery-field prerender-rum
                                                  preloaded-values]]
            [thermos-pages.map-import-components :as comps]))

(defn create-map-form [project-id]
  (let [start-state (assoc comps/start-state :project-id project-id)]
    (page
     {:title "Create a new map"
      :js ["/js/database_import.js"]
      :css ["/css/create-map-form.css"]
      :preload {:initial-state start-state}}
     (prerender-rum (comps/map-creation-form (atom start-state))))))

(defn delete-map-page []
  (page {:title "Delete map"}
        [:div
         "Do you really want to delete this map?"]
        [:div
         "Deleting the map will delete all of the network designs that use it."]
        [:div
         [:form {:method :POST}
          [:input {:type :submit :value "DELETE"}]]]))

(ns thermos-backend.pages.maps
  (:require [thermos-backend.pages.common :refer [page anti-forgery-field prerender-rum
                                                  preloaded-values]]
            [thermos-pages.map-import-components :as comps]))

(defn create-map-form [& [map-id]]
  (page
   {:title "Create a new map"
    :js ["/js/database_import.js"]
    :css ["/css/create-map-form.css"]
    :preload {:initial-state comps/start-state}}
   (prerender-rum (comps/map-creation-form (atom comps/start-state)))))

(defn delete-map-page []
  (page {:title "Delete map"}
        [:div
         "Do you really want to delete this map?"]
        [:div
         "Deleting the map will delete all of the network designs that use it."]
        [:div
         [:form {:method :POST}
          [:input {:type :submit :value "DELETE"}]]]))

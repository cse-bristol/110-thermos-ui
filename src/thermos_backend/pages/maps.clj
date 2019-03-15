(ns thermos-backend.pages.maps
  (:require [thermos-backend.pages.common :refer [page anti-forgery-field]]))

(defn create-map-form []
  (page
   {:title "Create a new map"
    :js ["/js/database_import.js"]}
   [:div
    [:form {:method :POST :enctype "multipart/formdata"}
     (anti-forgery-field)
     [:div.card
      [:label "Map name: "
       [:input {:name "map-name"
                :placeholder "My map"}]]
      [:label "Map description: "
       [:input {:name "map-description"
                :placeholder "A map for my city"}]]]
     
     ;; The below is the bit produced by the js
     [:div#form]]]))

(defn delete-map-page []
  (page {:title "Delete map"}
        [:div
         "Do you really want to delete this map?"]
        [:div
         "Deleting the map will delete all of the network designs that use it."]

        [:div
         [:form {:method :POST}
          [:input {:type :submit :value "DELETE"}]]
         
         ]
        ))



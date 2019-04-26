(ns thermos-backend.pages.admin
  (:require [thermos-backend.pages.common :refer [page]]))

(defn admin-page [users queues]
  (page
   {:title "THERMOS admin"}
   [:div.card
    [:div [:h1 "System users"]]
    [:table
     [:thead
      [:tr
       [:th "ID"]
       [:th "Name"]
       [:th "Authority"]
       [:th "Has logged in"]]]
     [:tbody
      (for [u users]
        [:tr
         [:td (:id u)]
         [:td (:name u)]
         [:td (:auth u)]
         [:td (not (nil? (:password u)))]])]]]

   (for [[q tasks] (group-by :queue-name queues)]
     [:div.card
      [:div.flex-cols
       [:h1.flex-grow q " tasks"]
       [:a {:href (str "clean-queue/" q)} "clean up"]
       " • "
       [:a {:href (str "clean-queue/" q "?purge=1")} "purge"]]
      
      [:table
       [:thead [:tr [:th "ID"] [:th "State"] [:th "Queued"] [:th "Updated"]]]
       
       [:tbody
        (for [t tasks]
          [:tr
           [:td (:id t)]
           [:td (:state t)]
           [:td (:queued t)]
           [:td (:updated t)]])]]])))

(defn send-email-page []
  (page
   {:title "Send email"
    :body-style { :margin "1em" :display "flex" :flex-direction "column"}}

   [:form.flex-rows.flex-grow {:method :POST}
    [:div.flex-rows.flex-grow
     [:input {:style "width:100%" :type :text :placeholder "Subject" :name "subject"}]
     [:textarea.flex-grow {:name "message"}]
     [:input.button {:type :submit}]]]))


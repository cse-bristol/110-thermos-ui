(ns thermos-backend.admin-pages
  (:require [hiccup.page :refer :all]
            [thermos-backend.pages :refer [thermos-page]]
            [thermos-backend.queue :as queue]
            [ring.util.anti-forgery :refer [anti-forgery-field]]
            ))

(defn job-list []
  (thermos-page
   {:title "System status"}
   [:div.container
    [:div.card
     [:h1 "Optimisation queue"]
     (let [jobs (queue/list-tasks :problems)]
       [:table.table
        [:thead
         [:tr
          [:th "ID"]
          [:th "State"]
          [:th "Submitted"]
          [:th "Updated"]]]
        [:tbody
         (for [job jobs]
           [:tr
            [:td (:id job)]
            [:td (:state job)]
            [:td (:queued job)]
            [:td (:updated job)]
            ]
           )]]
       )
     ]
    [:div.card
     [:h1 "Import queue"]
     (let [jobs (queue/list-tasks :imports)]
       [:table.table
        [:thead
         [:tr
          [:th "ID"]
          [:th "State"]
          [:th "Submitted"]
          [:th "Updated"]]]
        [:tbody
         (for [job jobs]
           [:tr
            [:td (:id job)]
            [:td (:state job)]
            [:td (:queued job)]
            [:td (:updated job)]]
           )]])]]
   ))

(defn database-import []
  (thermos-page
   {:title "Database import"
    :js ["/js/database_import.js"]}
   
   [:form {:method :POST :enctype "multipart/form-data"}
    (anti-forgery-field)
    [:div#form]]))

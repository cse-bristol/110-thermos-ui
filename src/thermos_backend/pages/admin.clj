(ns thermos-backend.pages.admin
  (:require [thermos-backend.pages.common :refer [page]]
            [clojure.pprint :refer [pprint]]))

(defn admin-page [users queues]
  (page
   {:title "THERMOS admin"}
   [:div.card
    [:details
     [:summary [:h1 "System users"]]
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
          [:td (not (nil? (:password u)))]])]]]]

   (for [[q tasks] (group-by :queue-name queues)]
     [:div.card
      [:details
       [:summary.flex-cols
        [:h1.flex-grow q " tasks"]
        [:a {:href (str "clean-queue/" q)} "clean up"]
        " • "
        [:a {:href (str "clean-queue/" q "?purge=1")} "purge"]]
       
       [:table {:style {:width "100%"}}
        [:thead [:tr [:th "ID"] [:th "State"] [:th "Queued"] [:th "Updated"]]]
        
        [:tbody
         (for [t tasks]
           [:tr
            [:td [:a {:href (str "job/" (:id t))} (:id t)]]
            [:td (:state t)]
            [:td (:queued t)]
            [:td (:updated t)]])]]]])))

(defn job-page [{:keys [id queue-name args state queued updated message progress]}]
  (page
   {:title (str "THERMOS admin - job " id)}
   [:div.card
    [:h1 "Job details for " id]
    [:table
     [:tr [:th "Queue"] [:td queue-name]]
     [:tr [:th "State"] [:td state]]
     [:tr [:th "Queued"] [:td queued]]
     [:tr [:th "Updated"] [:td updated]]
     [:tr [:th "Progress"] [:td progress]]]

    [:details
     [:summary [:h1 "Message"]]
     [:pre message]]

    [:details
     [:summary [:h1 "Arguments"]]
     [:pre (with-out-str (pprint args))]]
    
    [:form {:method :POST}
     [:button {:name "action" :value "restart" :type "submit"} "Re-run job"]
     [:button {:name "action" :value "cancel" :type "submit"}  "Cancel job"]]]
   )
  )

(defn send-email-page []
  (page
   {:title "Send email"
    :body-style { :margin "1em" :display "flex" :flex-direction "column"}}

   [:form.flex-rows.flex-grow {:method :POST}
    [:div.flex-rows.flex-grow
     [:input {:style "width:100%" :type :text :placeholder "Subject" :name "subject"}]
     [:textarea.flex-grow {:name "message"}]
     [:input.button {:type :submit}]]]))


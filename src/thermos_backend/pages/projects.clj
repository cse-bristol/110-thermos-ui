(ns thermos-backend.pages.projects
  (:require [thermos-backend.pages.common :refer [page]]
            [thermos-pages.common :refer [style]]
            [hiccup.util :refer [raw-string]]
            [thermos-pages.project-components :refer [project-page-body
                                                      delete-project-widget]]
            [rum.core :as rum]
            [thermos-backend.config :refer [config]]))

(defn new-project-page []
  (page
   {:title "New Project"}
   [:form.flex-rows {:method :POST}
    [:input
     {:style (style :border "1px #aaa solid"
                    :font-size :2em
                    :border-radius :5px
                    :padding :5px
                    :margin-bottom :0.5em)
      :required "required"
      :minlength 1
      :pattern ".*[^ ].*"
      :name "name" :type "text" :placeholder "Project name"}]
    [:div
     [:textarea {:style (style :width :100%
                               :padding :5px
                               :font-family :Sans)
                 :name "description" :rows 5 :placeholder "Project description"}]]
    [:div {:style (style :margin-bottom :0.5em)}
     [:div "Project members"]
     [:textarea {:style (style :width :100%
                               :padding :5px
                               :font-family :Sans)
                 :name "members" :rows 5 :placeholder "Joe Smith <joe@smith.com>"}]]
    [:div.flex-rows
     [:input.button
      {:style (style :margin-left :auto)
       :type :submit :value "Create Project"}]]]))

(defn project-page [project]
  ;; TODO filter out delete links for non-admin users
  (page {:title (:name project) :js ["/js/projects.js"]}
        (raw-string
         (rum/render-html (project-page-body project)))))

(defn delete-project-page [project wrong-name]
  (page {:title (str "Delete " (:name project) "?")}
        [:form {:method :POST}
         (raw-string
          (rum/render-static-markup
           (delete-project-widget {}  project wrong-name)))
         [:input.button.button--danger {:type :submit :value "DELETE"}]]))


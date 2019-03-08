(ns thermos-backend.pages.projects
  (:require [thermos-backend.pages.common :refer [page]]
            [thermos-pages.common :refer [style]]
            [thermos-pages.projects :refer [project-page-body]]))

(defn new-project-page []
  (page
   {:title "New Project"}
   [:form.flex-rows {:method :POST
           
           }
    [:input
     {:style (style :border "1px #aaa solid"
                    :font-size :2em
                    :border-radius :5px
                    :padding :5px
                    :margin-bottom :0.5em
                    )

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
        (project-page-body project)))

(defn delete-project-page [project wrong-name]
  (page {:title (str "Delete " (:name project) "?")}
        [:div "Do you really want to delete this project?"]
        [:div "This will delete everything related to it:"
         [:ul
          (for [m (:maps project)]
            [:li [:b (:name m)] " (map)"
             [:ul
              (for [n (:networks m)]
                [:li [:b (:name n)] " (network)"])]])]]
        [:div
         {:style (style :margin-bottom :1em)}
         "If you really want to delete the project enter its name "
         [:span {:style (style :font-family :Monospace
                               :font-size :2em
                               :border "1px #aaa solid"
                               :background "#fff")}
          (:name project)]
         " below and click delete"]
        (when wrong-name
          [:div "The project name was not entered correctly. "
           "You need to enter the project name below to delete the project."])
        [:div
         [:form {:method :POST}
          [:input {:style (style :font-size :2em
                                 :background "#faa"
                                 :color :black)
                   :type :text :name :project-name}]
          [:input.button.button--danger {:type :submit :value "DELETE"}]]]
        )
  )

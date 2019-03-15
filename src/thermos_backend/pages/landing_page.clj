(ns thermos-backend.pages.landing-page
  (:require [thermos-backend.pages.common :refer [page]]))

(defn- project-card [project]
  [:div.card
   [:h1 [:a {:href (str "/project/" (:id project))} (:name project)]]
   [:p (:description project)]
   [:span "Project facts, project facts"]
   ])

(defn landing-page [user projects]
  (page
   {:title (str "Welcome " user)}
   [:div.container
    [:div.left-area
     (if (seq projects)
       (for [project projects]
         (project-card project)
         )
       [:div
        [:h1 "You have no projects"]
        [:div "Click " [:em "New Project"] " to begin"]
        ]
       )
     ]
    [:div.right-area
     [:a.button {:href "/project/new"} "New Project"]
     ]]
   ))

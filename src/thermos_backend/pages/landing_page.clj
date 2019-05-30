(ns thermos-backend.pages.landing-page
  (:require [thermos-backend.pages.common :refer [page]]
            [clojure.string :as string]))

(defn- project-card [project]
  [:div.card {:style {:flex 1}}
   [:h1 [:a {:href (str "/project/" (:id project))}
         (if (string/blank? (:name project))
           "The project with no name"
           (:name project)
           )]]
   (when-not (string/blank? (:description project))
     [:p (:description project)])])

(defn landing-page [user projects]
  (page
   {:title (str "Welcome " user)}
   [:div
    [:div.flex-cols.card
     (if (seq projects)
       [:h1 "You are participating in " (count projects) " projects"]
       [:h1 "You have no projects"])
     [:a.button {:style {:margin-left "auto"}
                 :href "/project/new"} "New Project"]]
    
    [:div {:style {:margin-top "1em"
                   :display "flex"
                   :flex-direction "row"
                   :flex-wrap "wrap"
                   }}
     (for [project projects]
       (project-card project)
       )
     ]
    ]
   ))

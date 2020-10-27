(ns thermos-backend.pages.landing-page
  (:require [thermos-backend.pages.common :refer [page]]
            [clojure.string :as string]
            [thermos-backend.changelog :refer [changelog]]
            ))

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
   {:title (str "Welcome, " (:name user))}
   [:div
    
    
    
    [:div.flex-cols.card
     (if (seq projects)
       (let [c (count projects)]
         [:h1 "You are participating in " c " project" (when (> c 1) "s")])
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
       )]

    (let [last-log (:changelog-seen user 0)
          max-log (count changelog)
          new-items (- max-log last-log)
          changelog (take new-items changelog)]
      (when (seq changelog)
        [:div {:style {:position "fixed"
                       :background "rgba(1,1,1,0.5)"
                       :left 0
                       :top 0
                       :width "100%"
                       :height "100%"
                       }}
         [:div.card {:style {:margin "5em"}}
          [:div.flex-cols
           [:h1 "THERMOS has been updated"]
           [:a.button {:href "/help/changelog"
                       :target "help"
                       :style {:margin-left "auto"}}
            "🛈 More details"]
           [:a.button {:href (str "?changes=" max-log)
                       :style {:margin-left "1em"}}
            "Close"]]
          
          [:p {:style {:margin-bottom 0}}
           "Now featuring: "
           (interpose " • "
                      (for [rel changelog
                            change (:changes rel)]
                        (:title change)
                        ))]]
         ]))
    ]
   ))

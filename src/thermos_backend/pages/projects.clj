(ns thermos-backend.pages.projects
  (:require [thermos-backend.pages.common :refer [page]]))

(defn new-project-page []
  (page
   {:title "New Project"}
   [:form {:method :POST}
    [:input {:name "name" :type "text" :placeholder "Project name"}]
    [:div
     [:textarea {:name "description" :rows 5 :placeholder "Project description"}]]
    [:div
     [:h2 "Project members"]
     ;; this needs some js glued onto it really
     [:textarea {:name "members" :rows 5 :placeholder "Joe Smith <joe@smith.com>"}]]
    [:div
     [:input {:type :submit :value "Create Project"}]]]))

(defn project-page [project]
  (page {:title (:name project)}
        [:div
         [:div.card (:description project)]
         (if-let [maps (seq (:maps project))]
           (for [m maps]
             [:div.card
              [:h1 (:name m)]
              [:span "Info about this map should go here, like a picture or bbox"]
              (if-let [problems (seq (:problems m))]
                (for [p problems]
                  [:div.card
                   [:h1 (:name p)]
                   [:span "Info about a network should go here perhaps"]
                   ])
                [:div
                 "This map has no network designs associated with it yet."])
              [:a {:href (str "map/" (:id m) "/network/new")}
               "New network"
               ]
              ])
           [:div
            "This project has no maps in it yet. "
            "Get started by creating a new map:"]
           
           )
         
         [:a {:href "map/new"} "New map"]
         ]
        )
  )

(defn new-map-page [project]
  (page {:title (str "New map for " (:name project))}
        [:div
         "this is the import page really"
         ]
        )
  )

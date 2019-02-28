(ns thermos-backend.pages.projects
  (:require [thermos-backend.pages.common :refer [page style]]))

(defn new-project-page []
  (page
   {:title "New Project"}
   [:form {:method :POST}
    [:input {:name "name" :type "text" :placeholder "Project name"}]
    [:div
     [:textarea {:name "description" :rows 5 :placeholder "Project description"}]]
    [:div
     [:h2 "Project members"]
     [:textarea {:name "members" :rows 5 :placeholder "Joe Smith <joe@smith.com>"}]]
    [:div
     [:input {:type :submit :value "Create Project"}]]]))

(defn project-page [project]
  (page {:title (:name project)}
        [:div
         [:div.card.flex-cols
          [:div {:style (style :flex-grow 1)}
           (:description project)]
          [:div [:a {:href "map/new"} "IMPORT NEW MAP"]]

          ]
         (if-let [maps (seq (:maps project))]
           (for [m maps]
             [:div.card
              [:div.flex-cols
               [:h1 {:style (style :flex-grow 1)}
                (:name m)]
               [:div
                [:a {:href (str "map/" (:id m) "/delete")} "DELETE MAP"]
                " • "
                [:a {:href (str "map/" (:id m) "/net/new")} "NEW NETWORK"]
                ]
               ]
              
              [:span (:description m)]
              [:img {:style (style :float :right)
                     :src (str "map/" (:id m) "/icon.png")
                     :alt (str "An image for the map " (:name m)) }]
              
              (if-let [networks (seq (:networks m))]
                [:div.flex-grid
                 (for [[name ns] (group-by :name networks)]
                   [:div.card
                    [:h1 [:a {:href (str
                                     "map/" (:id m)
                                     "/net/" (reduce max (map :id ns)))} name]]
                    [:span
                     (count ns) " version"
                     (if (seq (rest ns)) "s" "")]
                    
                    ])]

                [:div
                 "This map has no network designs associated with it yet."])

              [:br {:style (style :clear :both)}]])
           [:div
            "This project has no maps in it yet. "
            "Get started by creating a new map."]
           
           )
         
         
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

(ns thermos-pages.projects
  (:require [thermos-pages.common :refer [style]]
            [clojure.string :as string])
  )

(defn project-page-body [project]
  (let [am-admin (:user-is-admin project)]
    [:div
     [:div.card
      [:div.flex-cols
       [:div {:style (style :flex-grow 1)}
        (let [d (:description project)]
          (if (string/blank? d)
            [:em "This project has no description"]
            [:span d]))]
       [:div
        (when am-admin
          [:span [:a {:href "delete"} "DELETE PROJECT"]
           " • "])

        [:a {:href "map/new"} "IMPORT NEW MAP"]]]
      
      
      ]
     [:div.card
      #?(:cljs {:on-click #(js/alert "HELLO")})
      [:b "Project users: "]
       (for [u (:users project)]
         [:span (:name u)]
         )
       ]
     
     (if-let [maps (seq (:maps project))]
       (for [m maps]
         [:div.card {:key (:id m)}
          [:div.flex-cols
           [:h1 {:style (style :flex-grow 1)}
            (:name m)]
           [:div
            [:a {:href (str "map/" (:id m) "/delete")} "DELETE MAP"]
            " • "
            [:a {:href (str "map/" (:id m) "/data.json")} "DOWNLOAD"]
            (if (:import-completed m)
              [:span
               " • "
               [:a {:href (str "map/" (:id m) "/net/new")} "NEW NETWORK"]])
            ]]
          
          [:span (:description m)]

          (if (:import-completed m)
            [:div
             [:img {:style (style :float :right)
                    :src (str "map/" (:id m) "/icon.png")
                    :alt (str "An image for the map " (:name m)) }]
             
             (if-let [networks (seq (:networks m))]
               [:div.flex-grid
                (for [[name ns] (group-by :name networks)]
                  [:div.card {:key name}
                   [:div
                    [:h1 [:a {:href (str
                                     "map/" (:id m)
                                     "/net/" (reduce max (map :id ns)))} name]]]
                   [:span
                    (count ns) " version"
                    (if (seq (rest ns)) "s" "")]
                   
                   ])]

               [:div
                "This map has no network designs associated with it yet."])

             [:br {:style (style :clear :both)}]]
            [:div "Map import in progress..."
             (:status m)
             ])])
       [:div.card
        "This project has no maps in it yet. "
        "Get started by creating a new map above."])]))


(ns thermos-ui.backend.pages
  (:require [compojure.core :refer :all]
            [thermos-ui.backend.store-service.problems :as problems]
            [hiccup.page :refer :all]
            [ring.util.response :refer [resource-response]]
            ))

(def source-sans-pro
  "https://fonts.googleapis.com/css?family=Source+Sans+Pro:400,400i,600,600i,700,700i")

(defroutes all
  (GET "/" []
       (html5
        {:title "THERMOS"}
        [:h1 "THERMOS"]
        [:p "Nothing here, you need to go see the editor"]))

  (GET "/:org-name/new" [org-name]
       (html5
        [:head
         [:meta {:charset "UTF-8"}]
         [:meta {:name :viewport :content "width=device-width, initial-scale=1"}]
         ;; TODO broke favicons
         (include-css "/css/editor.css" source-sans-pro)
         ]
        [:body
         [:div#app
          [:h1 "Loading, please wait"]]
         (include-js "/js/editor.js")]))


  (GET "/:org-name/" [org-name]
       (let [org-problems (problems/gather org-name)]
         (html5
          [:head [:title (str org-name)]]
          [:body
           [:a {:href "new"} "New problem"]
           [:ol
            (for [{id :id loc :location} org-problems]
              [:li id loc]
              )
            ]]))))

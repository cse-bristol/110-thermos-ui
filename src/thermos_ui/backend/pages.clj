(ns thermos-ui.backend.pages
  (:require [compojure.core :refer :all]
            [thermos-ui.backend.store-service.problems :as problems]
            [hiccup.page :refer :all]
            [ring.util.response :refer [resource-response]]
            ))

(def source-sans-pro
  "https://fonts.googleapis.com/css?family=Source+Sans+Pro:400,400i,600,600i,700,700i")

(def editor
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

;; TODO can these be directly related to the API by using accepts
;; header?

(defroutes all
  (GET "/" []
       (html5
        {:title "THERMOS"}
        [:h1 "THERMOS"]
        [:p "Nothing here, you need to go see the editor"]))

  (GET "/:org-name/:problem/:version/" [org-name problem version]
       editor)

  (GET "/:org-name/new" [org-name]
       editor)

  (GET "/:org-name/" [org-name]
       (let [org-problems
             (filter (comp (partial = org-name) :org)
                     (problems/ls org-name))

             org-problems (group-by :name org-problems)
             ]
         (html5
          [:head [:title (str org-name)]]
          [:body
           [:a {:href "new"} "New problem"]
           [:p "Existing problems for " org-name]
           [:ol
            (for [[name saves] org-problems]

              [:li [:a {:href (str name "/" (:id (apply max-key :date
                                                        saves)) "/")} name]])
            ]]))))

(ns thermos-backend.pages
  (:require [compojure.core :refer :all]
            [thermos-backend.problems.db :as problems]
            [hiccup.page :refer :all]
            [ring.util.response :refer [resource-response]]
            ))


(declare delete-problem-modal-html thermos-page)

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

(defn- format-date [date]
  (.format (java.text.SimpleDateFormat. "dd/MM/yyyy - HH:mm") (java.util.Date. date)))

(def all
  (routes
   (GET "/" []
        (html5
         {:title "THERMOS"}
         [:h1 "THERMOS"]
         [:p "Nothing here, you need to go see the editor"]))

   (GET "/:org-name/:problem/:version" [org-name problem version]
        editor)

   (GET "/:org-name/new" [org-name]
        editor)

   (GET "/:org-name/:problem" [org-name problem]
        "Redirect to the latest version if no version given."
        (let [problem-coll
              (problems/ls org-name problem)

              latest-version-id (->> problem-coll
                                     (sort-by :created)
                                     last
                                     :id)]
          (if latest-version-id
            (ring.util.response/redirect (str "/" org-name "/" problem "/" latest-version-id))
            (ring.util.response/redirect (str "/" org-name)))
          ))

   (GET "/:org-name" [org-name]
        (let [org-problems
              (problems/ls org-name)

              org-problems (group-by :name org-problems)
              ]
          (thermos-page
           {:title org-name :js ["/js/problems_list.js"]}
           [:div.top-banner 
            [:div.container
             {:style "display:flex;align-items:center;"}

             "Saved Problems"
             [:a.button {:href (str "/" org-name "/new")
                         :style "margin-left:auto;"
                         } "NEW +"]]
            
            ]
           [:div.container
            [:div.card
             [:table.table
              [:thead
               [:tr
                [:th "Title"]
                [:th {:style "width:150px;"} "Last solution"]
                [:th {:style "width:150px;"} "Last updated"]
                [:th {:style "width:100px;"} ""]]]
              [:tbody
               (let [rows
                     (for [[name saves] org-problems]
                       {:latest (apply max-key :created saves)
                        :solution (->> saves (filter :has_run) (sort-by :created) (last))
                        :name name})

                     rows (sort-by (juxt (comp :created :latest)
                                         (comp :created :solution)
                                         :name)
                                   rows)

                     rows (reverse rows)
                     ]

                 (for [{name :name
                        {latest-id :id latest-date :created} :latest
                        {solved-id :id solved-date :created} :solution
                        } rows]
                   [:tr
                    [:td [:a.link {:href (str org-name "/" name "/" latest-id "/")} name]]
                    [:td (when solved-id
                           [:a.link {:href (str org-name "/" name "/" solved-id)}
                            (format-date solved-date)])]
                    [:td {:data-saved-timestamp latest-date} (format-date latest-date)]
                    [:td {:style "text-align:right;"}
                     [:button.button.button--small
                      {:data-action "delete-problem" :data-org org-name :data-problem-name name}
                      "Delete"]]]))
               ]]
             
             ]]
           (for [[name saves] org-problems]
             (delete-problem-modal-html name))
           )
          ))))

(defn delete-problem-modal-html
  "Returns the modal html for confirming deletion of a problem."
  [problem-name]
  (html5
   [:div.modal {:data-problem-name problem-name}
   [:div.modal__header "Confirm" [:button.button--modal-close {:data-action "modal-close"} "×"]]
   [:div.modal__content
    [:p "Are you sure you want to delete this problem?"]
    [:br]
    [:button.button.button--modal-cancel {:data-action "modal-close"} "Cancel"]
    [:button.button.button--modal-confirm {:data-action "modal-confirm"} "Delete"]
    ]]))

(defn thermos-page
  [{title :title
    js :js
    css :css}
   & contents]
  (html5
   [:head
    [:title (str "THERMOS: " title)]
    [:meta {:charset "UTF-8"}]
    [:meta {:name :viewport :content "width=device-width, initial-scale=1"}]
    (include-css "/css/pages.css" source-sans-pro)
    ]
   [:body
    [:header.page-header
     [:div.container
      [:h1 "THERMOS - " title]]]
    [:div.page-content
     contents]
    [:footer.page-footer
     [:div.container "Some footer stuff"]]
    ;; Include the JS at the bottom. This ensures that any elements that are needed
    ;; are rendered by the time the file is loaded.
    (for [js-file js]
      (include-js js-file))
    ]))

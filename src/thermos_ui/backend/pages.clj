(ns thermos-ui.backend.pages
  (:require [compojure.core :refer :all]
            [thermos-ui.backend.store-service.problems :as problems]
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
         (thermos-page
          {:title (str org-name)
           :js ["/js/problems_list.js"]}
          [:div.top-banner
           [:div.container "Saved Problems"]]
          [:div.container
           [:div.card
            [:table.table
             [:thead
              [:tr
               [:th "Title"]
               [:th {:style "width:150px;"} "Last updated"]
               [:th {:style "width:100px;"} ""]]]
             [:tbody
              (for [[name saves] org-problems]
                (let [latest-save (apply max-key :date saves)
                      latest-save-date (.format (java.text.SimpleDateFormat. "dd/MM/yyyy - HH:mm")
                                                (java.util.Date. (:date latest-save)))
                      ]
                  [:tr
                   [:td [:a.link {:href (str name "/" (:id latest-save) "/")} name]]
                   [:td {:data-saved-timestamp (:date latest-save)} latest-save-date]
                   [:td {:style "text-align:right;"}
                    [:button.button.button--small
                     {:data-action "delete-problem" :data-org org-name :data-problem-name name}
                     "Delete"]]]))
              ]]
            [:br]
            [:a.button {:href "new"} "NEW +"]
            ]]
          (for [[name saves] org-problems]
            (delete-problem-modal-html name))
          )
         )))

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
    [:title title]
    [:meta {:charset "UTF-8"}]
    [:meta {:name :viewport :content "width=device-width, initial-scale=1"}]
    (include-css "/css/pages.css" source-sans-pro)
    ]
   [:body
    [:header.page-header
     [:div.container
      [:h1 "THERMOS"]]]
    [:div.page-content
     contents]
    [:footer.page-footer
     [:div.container "Some footer stuff"]]
    ;; Include the JS at the bottom. This ensures that any elements that are needed
    ;; are rendered by the time the file is loaded.
    (for [js-file js]
      (include-js js-file))
    ]))

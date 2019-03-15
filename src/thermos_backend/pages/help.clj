(ns thermos-backend.pages.help
  (:require [thermos-backend.pages.common :refer [page]]))

(defn help-page []
  (page
   {:title "Help"
    :js ["/js/help.js"]
    :css ["/css/help.css"]
    :body-style {:display "flex"}}
   [:div#help-app.flex-grow
    [:div#help-menu-panel]
    [:div#help-content-panel]]))


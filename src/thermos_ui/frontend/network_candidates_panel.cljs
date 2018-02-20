(ns thermos-ui.frontend.network-candidates-panel
  (:require [reagent.core :as reagent]
            [thermos-ui.frontend.tag :as tag]))

(defn component
  "DOCSTRING"
  [document]
  (reagent/create-class
   {:reagent-render (fn [document]
                      [:div
                       [:nav.nav.nav--sub-nav
                        [:h2.nav__header "Network Candidates"]]
                       [:table.table
                        [:tbody
                         [:tr
                          [:td "Lorem ipsum"]
                          [:td
                           "Lorem ipsum dolor sit amet"]]
                         [:tr
                          [:td "Lorem ipsum"]
                          [:td
                           "Lorem ipsum dolor sit amet"]]
                         [:tr
                          [:td "Lorem ipsum"]
                          [:td
                           "Lorem ipsum dolor sit amet"]]
                         [:tr
                          [:td "Lorem ipsum"]
                          [:td
                           "Lorem ipsum dolor sit amet"]]
                         [:tr
                          [:td "Lorem ipsum"]
                          [:td
                           "Lorem ipsum dolor sit amet"]]]]])}))

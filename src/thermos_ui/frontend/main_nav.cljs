(ns thermos-ui.frontend.main-nav
  (:require [reagent.core :as reagent]))

(defn component
  "The main nav bar at the top of the page."
  [document]
  [:nav.nav
   [:h1.main-nav__header "THERMOS"
    [:span "Heat Network Editor"]]
   [:div.main-nav__input-container
    [:input.text-input.main-nav__file-name-input {:type "text" :placeholder "Untitled"}]]
   [:div.pull-right.main-nav__input-container
    [:button.button.button--link-style.button--save-button "SAVE"]
    [:button.button.button--link-style.button--load-button "LOAD"]
    [:button.button.button--outline.main-nav__run-button "RUN" [:span "▸"]]]])

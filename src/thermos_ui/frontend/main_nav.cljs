(ns thermos-ui.frontend.main-nav
  (:require [reagent.core :as reagent]))

(defn component
  "The main nav bar at the top of the page."
  [{ name :name }]
  (let [state (reagent/atom {:name name})]
    (fn [{on-save :on-save
          on-load :on-load
          on-run :on-run
          }]
      [:nav.nav
       [:h1.main-nav__header "THERMOS"
        [:span "Heat Network Editor"]]
       [:div.main-nav__input-container
        [:input.text-input.main-nav__file-name-input
         {:type "text" :placeholder "Untitled"
          :value (:name @state)
          :on-change #(swap! state assoc :name (.. % -target -value))}]]

       [:div.pull-right.main-nav__input-container
        [:button.button.button--link-style.button--save-button
         {:on-click #(on-save (:name @state))}
         "SAVE"]
        [:button.button.button--link-style.button--load-button {:on-click on-load} "LOAD"]
        [:button.button.button--outline.main-nav__run-button {:on-click on-run}
         "RUN" [:span "▸"]]]])))

(ns thermos-ui.frontend.main-nav
  (:require [reagent.core :as reagent]
            [thermos-ui.specs.document :as document]
            [clojure.string :as s]))

(defn component
  "The main nav bar at the top of the page."
  [{ name :name }]
  (let [state (reagent/atom {:name name})]
    (fn [{on-save :on-save
          on-load :on-load
          on-run :on-run
          unsaved? :unsaved?
          }]
      [:nav.nav
       [:h1.main-nav__header "THERMOS"
        [:span "Heat Network Editor"]]
       [:div.main-nav__input-container
        [:input.text-input.main-nav__file-name-input
         {:type "text" :placeholder "Untitled"
          :value (:name @state)
          :id "file-name-input"
          :on-key-press #(.stopPropagation %) ;; Prevent keyboard shortcuts from executing when you type
          :on-change #(swap! state assoc :name (.. % -target -value))
          ;; If this is a new problem, focus on the project name input
          :ref (fn [element]
                 (when (= (:name @state) "")
                   (.focus element)))}]]

       [:div.pull-right.main-nav__input-container
        [:button.button.button--link-style.button--save-button
         {:class (if-not @unsaved? "button--disabled")
          :on-click #(let [name (:name @state)]
                       ;; If this is a new probem and a name has not been provided, prompt user to do so.
                       (if (and (some? name) (not (s/blank? name)))
                         (on-save name)
                         (do (js/window.alert "Please provide a name for this project.")
                           (.focus (js/document.getElementById "file-name-input")))))
          }
         "SAVE"]
        [:button.button.button--link-style.button--load-button {:on-click on-load} "LOAD"]
        [:button.button.button--outline.main-nav__run-button {:on-click on-run}
         "RUN" [:span "▸"]]]])))

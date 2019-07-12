(ns thermos-frontend.main-nav
  (:require [reagent.core :as reagent]
            [thermos-specs.document :as document]
            [thermos-frontend.theme :as theme]
            [clojure.string :as s]))

(defn component
  "The main nav bar at the top of the page."
  [{name :name
    on-save :on-save
    on-run :on-run
    unsaved :unsaved?
    hamburger :hamburger
    }]
  (reagent/with-let [state (reagent/atom {:name name})
                     element (atom nil)
                     with-name (fn [act]
                                 (let [{name :name} @state
                                       el @element]
                                   (if (and (some? name) (not (s/blank? name)))
                                     (act name)
                                     (do (js/window.alert "Please provide a name for this project.")
                                         (.focus el)))))
                     ]
    [:nav.nav {:style {:display :flex}}
     hamburger
     
     [:span {:style {:display :flex
                     :margin-left :2em
                     :margin-right :2em
                     :align-items :center
                     :flex 1}}
      [:h1 {:style {:margin-right :0.5em}} "THERMOS"]
      [:input.text-input.main-nav__file-name-input
       {:type :text :placeholder "Untitled"
        :on-change #(swap! state assoc :name (.. % -target -value))
        :style {:flex 1}
        :value (:name @state)
        :ref (fn [e]
               (reset! element e)
               (when e
                 (.addEventListener
                  e "keypress"
                  (fn [e] (.stopPropagation e))
                  false)
                 (when-not (:name @state) (.focus e))))
        }]
      ]
     
     [:span {:style {:display :flex :margin-left :auto}}
      (when unsaved
        [:button.button.button--outline.button--save-button
         {:style {:background "none" :border "none"}
          :on-click #(with-name on-save)}
         "Save"
         ])
      [:button.button.button--outline
       {:style {:background "none" :border "none"}
        :on-click #(with-name on-run)}
       "Optimise ▸"
       ]]
     ]))

(ns thermos-ui.frontend.main-nav
  (:require [reagent.core :as reagent]
            [thermos-ui.specs.document :as document]
            [clojure.string :as s]))

(defn component
  "The main nav bar at the top of the page."
  [{name :name
    tabs :tabs
    selected-tab :selected-tab
    on-save :on-save
    on-run :on-run
    on-tab-switch :on-tab-switch
    unsaved :unsaved
    }]

  (reagent/with-let [state (reagent/atom {:name name})
                     with-name (fn [act]
                                 (let [{name :name el :element} @state]
                                   (if (and (some? name) (not (s/blank? name)))
                                     (act name)
                                     (do (js/window.alert "Please provide a name for this project.")
                                         (.focus el)))))
                     ]
    
    [:nav.nav {:style {:display :flex}}
     [:span {:style {:display :flex :margin-right :auto}}
      (for [tab (filter identity tabs)]

        [(if (= (:key tab) selected-tab)
           :button.button--tab.button--tab--selected
           :button.button--tab
           )
         {:key (:key tab)
          :on-click #(on-tab-switch (:key tab))
          }
         (:label tab)]
        )
      ]

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
        :on-key-press #(.stopPropagation %)
        :ref (fn [element]
               (swap! state assoc :element element)
               (when-not (:name @state)
                 (.focus element)))
        }]
      ]
     
     [:span {:style {:display :flex :margin-left :auto}}
      [:button.button.button--outline.button--save-button
       {:style {:background "none" :border "none"}
                :on-click #(with-name on-save)
                }
       "Save"
       ]
      [:button.button.button--outline
       {:style {:background "none" :border "none"}
                :on-click #(with-name on-run)
                }
       "Optimise ▸"
       ]]
     ]))

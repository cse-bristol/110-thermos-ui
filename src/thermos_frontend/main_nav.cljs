(ns thermos-frontend.main-nav
  (:require [reagent.core :as reagent]
            [thermos-specs.document :as document]
            [clojure.string :as s]))

(defn component
  "The main nav bar at the top of the page."
  [{name :name
    tabs :tabs
    selected-tab :selected-tab
    on-save :on-save
    on-run :on-run
    on-tab-switch :on-tab-switch
    unsaved :unsaved?
    }]

  (reagent/with-let [state (reagent/atom {:name name})
                     element (atom nil)
                     with-name (fn [act]
                                 (let [{name :name} @state
                                       el @element
                                       ]
                                   (if (and (some? name) (not (s/blank? name)))
                                     (act name)
                                     (do (js/window.alert "Please provide a name for this project.")
                                         (.focus el)))))
                     ]
    [:nav.nav {:style {:display :flex}}
     [:span {:style {:display :flex :margin-right :auto}}
      (for [tab (filter identity tabs)]

        [(cond
           (:href tab) :a
           (= (:key tab) selected-tab) :button.button--tab.button--tab--selected
           :else :button.button--tab)
         
         (cond
           (:href tab) {:key (:key tab)
                        :href (:href tab) :target "_blank"
                        :style {:display :flex}}
           :else {:key (:key tab) :on-click #(on-tab-switch (:key tab))})

         (cond
           (:href tab) [:button.button--tab (:label tab)]
           :else (:label tab))
         
         ]
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
        :ref (fn [e]
               (reset! element e)
               (when e
                 (when-not (:name @state) (.focus e))))
        }]
      ]
     
     [:span {:style {:display :flex :margin-left :auto}}
      (when unsaved
        [:button.button.button--outline.button--save-button
         {:style {:background "none" :border "none"}
          :on-click #(with-name on-save)
          ;; :disabled (not unsaved)
          }
         "Save"
         ])
      [:button.button.button--outline
       {:style {:background "none" :border "none"}
                :on-click #(with-name on-run)
                }
       "Optimise ▸"
       ]]
     ]))

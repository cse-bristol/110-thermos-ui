;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

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
    read-only :read-only
    }]
  (reagent/with-let [name-text (reagent/atom name)
                     optimise-clicked (reagent/atom false)
                     tools-clicked (reagent/atom false)
                     click-optimise #(reset! optimise-clicked true)
                     click-tools #(reset! tools-clicked true)
                     
                     element (atom nil)
                     with-name (fn [act]
                                 (let [name-text @name-text
                                       el @element]
                                   (if (s/blank? name-text)
                                     (do (js/window.alert "Please provide a name for this project.")
                                         (.focus el))
                                     (act name-text))))
                     ]
    [:nav.nav {:style {:display :flex}}
     hamburger
     
     [:span {:style {:display :flex
                     :margin-left :2em
                     :margin-right :2em
                     :align-items :center
                     :flex 1}}
      [:h1 {:style {:margin-right :0.5em}} "THERMOS"]
      (if read-only
        [:b @name-text]
        [:input.text-input.main-nav__file-name-input
         {:type :text :placeholder "Untitled"
          :on-change #(reset! name-text (.. % -target -value))
          :style {:flex 1}
          :value @name-text
          :ref (fn [e]
                 (reset! element e)
                 (when e
                   (.addEventListener
                    e "keypress"
                    (fn [e] (.stopPropagation e))
                    false)
                   (when (and
                          (not (= "input" js/document.activeElement.nodeName))
                          (not @name-text)) (.focus e))))
          }])
      ]
     
     (when-not read-only
       (cond
         @optimise-clicked
         [:span.fade-in {:key :b :style {:display :flex :margin-left :auto}
                         :on-mouse-leave #(reset! optimise-clicked false)
                         }
          [:button.button.button--outline
           {:style {:background "none" :border "none"}
            :on-click #(reset! optimise-clicked false)} "◀"]
          
          [:button.button.button--outline
           {:style {:background "none" :border "none"}
            :on-click #(do
                         (reset! optimise-clicked false)
                         (on-run @name-text :network))}
           "Network"]
          [:button.button.button--outline
           {:style {:background "none" :border "none"}
            :on-click #(do (reset! optimise-clicked false)
                           (on-run @name-text :supply))}
           "Supply"]
          [:button.button.button--outline
           {:style {:background "none" :border "none"}
            :on-click #(do (reset! optimise-clicked false)
                           (on-run @name-text :both))}
           "Both"]]
         
         @tools-clicked
         [:span.fade-in {:key :b :style {:display :flex :margin-left :auto}
                         :on-mouse-leave #(reset! optimise-clicked false)
                         }
          [:button.button.button--outline
           {:style {:background "none" :border "none"}
            :on-click #(reset! tools-clicked false)} "◀"]
          
          [:button.button.button--outline
           {:style {:background "none" :border "none"}
            :on-click #(do
                         (reset! tools-clicked false)
                         (on-run @name-text :tidy))}
           "Tidy"]
          [:button.button.button--outline
           {:style {:background "none" :border "none"}
            :on-click #(do (reset! tools-clicked false)
                           (on-run @name-text :tree))}
           "Tree"]]
         
         :else
         [:span {:key :a :style {:display :flex :margin-left :auto}}
          (when unsaved
            [:button.button.button--outline.button--save-button
             {:style {:background "none" :border "none"}
              :on-click #(with-name on-save)}
             "Save"
             ])
          [:button.button.button--outline
           {:style {:background "none" :border "none"}
            :on-click #(with-name click-tools)}
           "⚙"]
          [:button.button.button--outline
           {:style {:background "none" :border "none"}
            :on-click #(with-name click-optimise)}
           "Optimise ▸"]]

         
         ))
     ]))

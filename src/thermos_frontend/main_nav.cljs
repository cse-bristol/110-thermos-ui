;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.main-nav
  (:require [reagent.core :as reagent]
            [re-frame.core :as rf]
            [thermos-specs.document :as document]
            [thermos-frontend.db :as db]
            [thermos-frontend.theme :as theme]
            [thermos-frontend.toaster :as toaster]
            [clojure.string :as s]))

(defonce ^:private current
  ;; the nav bar's name text atom and input, for ::rename-as-copy
  (atom nil))

;; Change the name in the nav bar to a `db/copy-name` of it not one of
;; the names given, and focus it, with the bit that was added selected,
;; and say why. (An effect, as the name is kept here, not in app-db.)
(rf/reg-fx
 ::rename-as-copy
 (fn [taken]
   (when-let [{:keys [name-text element]} @current]
     (let [old-name @name-text
           [new-name start end] (db/copy-name old-name taken)]
       (reset! name-text new-name)
       (reagent/after-render
        (fn []
          (when-let [^js el @element]
            (.focus el)
            (.setSelectionRange el start end))))
       (toaster/show!
        [:div.toaster
         "“" old-name "” is still being optimised, and you can find its result in the project. "
         "You are now editing a copy, which will be saved as “" new-name "”."]
        :duration 8)))))

(defn dropdown
  "A button saying `label` which opens a menu of `items` under it. The
  menu closes when something in it is clicked, on a click outside it, or
  on Escape."
  [{:keys [label title]} & items]
  (reagent/with-let [open (reagent/atom false)
                     node (atom nil)
                     close! #(reset! open false)
                     on-mouse-down (fn [e]
                                     (when-let [^js n @node]
                                       (when-not (.contains n (.-target e)) (close!))))
                     on-key-down (fn [e] (when (= "Escape" (.-key e)) (close!)))
                     _ (js/document.addEventListener "mousedown" on-mouse-down)
                     _ (js/document.addEventListener "keydown" on-key-down)]
    [:div.dropdown {:ref #(reset! node %)}
     [:button.button.button--outline
      {:style {:background "none" :border "none"}
       :class (when @open "dropdown__button--open")
       :title title
       :on-click #(swap! open not)}
      label " ▾"]
     (when @open
       (into
        [:div.dropdown__menu
         {:on-click (fn [e]
                      (when (.closest (.-target e) "button, a") (close!)))}]
        items))]
    (finally
      (js/document.removeEventListener "mousedown" on-mouse-down)
      (js/document.removeEventListener "keydown" on-key-down))))

(defn with-name
  "Call `act` with the name in the nav bar, or if it is blank, ask for one"
  [act]
  (when-let [{:keys [name-text element]} @current]
    (let [name-text @name-text]
      (if (s/blank? name-text)
        (do (js/window.alert "Please provide a name for this project.")
            (when-let [^js el @element] (.focus el)))
        (act name-text)))))

(defn component
  "The main nav bar at the top of the page."
  [{name :name
    on-save :on-save
    on-run :on-run
    unsaved :unsaved?
    ;; goes before the name, linking back to the project
    home :home
    ;; `dropdown`s, which are there even when read-only
    menus :menus
    read-only :read-only
    ;; a parameter set can be saved, but not optimised
    parameter-set :parameter-set
    }]
  (reagent/with-let [name-text (reagent/atom name)
                     optimise-clicked (reagent/atom false)
                     click-optimise #(reset! optimise-clicked true)

                     element (atom nil)
                     _ (reset! current {:name-text name-text :element element})
                     ]
    [:nav.nav.main-nav {:style {:display :flex}}
     home
     
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

     (into [:span.main-nav__menus] menus)

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
         
         :else
         [:span {:key :a :style {:display :flex :margin-left :auto}}
          [:button.button.button--outline
           {:style {:background "none" :border "none"}
            :title "Undo (Ctrl + z)"
            :disabled (not @(rf/subscribe [:undo/can-undo]))
            :on-click #(rf/dispatch [:doc/undo])}
           "↶"]
          [:button.button.button--outline
           {:style {:background "none" :border "none"}
            :title "Redo (Ctrl + y)"
            :disabled (not @(rf/subscribe [:undo/can-redo]))
            :on-click #(rf/dispatch [:doc/redo])}
           "↷"]
          (when unsaved
            [:button.button.button--outline.button--save-button
             {:style {:background "none" :border "none"}
              :on-click #(with-name on-save)}
             "Save"
             ])
          (when-not parameter-set
            [:button.button.button--outline
             {:style {:background "none" :border "none"}
              :on-click #(with-name click-optimise)}
             "Optimise ▸"])]

         
         ))
     ]))

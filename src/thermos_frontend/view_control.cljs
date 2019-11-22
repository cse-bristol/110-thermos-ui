(ns thermos-frontend.view-control
  (:require [thermos-specs.view :as view]
            [thermos-frontend.editor-state :as state]
            [thermos-frontend.theme :as theme]
            [reagent.core :as r]
            ))

(defn- label [colour label]
  [:label {:style {:margin-right :0.3em}}
   [:span {:style {:border-radius :4px
                   :width :8px
                   :height :8px
                   :margin-right :0.1em
                   :background colour
                   :display :inline-block}}]
   label
   ])

(defn- legend [map-view]
  (case map-view
    ::view/constraints
    [:span {:style {:margin-left :auto}}
     [label theme/blue "Optional"]
     [label theme/red "Required"]
     [label theme/supply-orange "Supply"]
     [label theme/light-grey "Forbidden"]
     ]

    ::view/solution
    [:span {:style {:margin-left :auto}}
     [label theme/in-solution-orange "Network"]
     [label theme/supply-orange "Supply"]
     [label theme/green "Individual"]
     [label theme/beige "Unused"]
     [label theme/magenta "Impossible"]
     ]
    nil))

(defn component [doc]
  (r/with-let [*map-view (r/cursor doc [::view/view-state ::view/map-view])]
    (let [map-view (or @*map-view ::view/constraints)]
      [:div.view-control.flex-cols
       [:h2.view-control__label "Show:"]
       [:button.view-control__button
        {:class (if (= ::view/constraints map-view) "view-control__button--active" "")
         :on-click (fn [] (state/edit! doc view/set-map-view ::view/constraints))}
        "Constraints"]
       [:button.view-control__button
        {:class (if (= ::view/solution map-view) "view-control__button--active" "")
         :on-click (fn [] (state/edit! doc view/set-map-view ::view/solution))}
        "Solution"]

       [legend map-view]
       ])))

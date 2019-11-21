(ns thermos-frontend.view-control
  (:require [reagent.core :as reagent]
            [thermos-specs.view :as view]
            [thermos-frontend.editor-state :as state]))

(defn- component [doc]
  (reagent/with-let
    [view (reagent/cursor doc [::view/view-state ::view/map-view])
     show-pipe-diameters? (reagent/cursor doc [::view/view-state ::view/show-pipe-diameters])]
    [:div.view-control
     [:h2.view-control__label "Map view:"]
     [:button.view-control__button
      {:class (if (= ::view/constraints @view) "view-control__button--active" "")
       :on-click (fn [] (state/edit! doc view/set-map-view ::view/constraints))}
      "Constraints"]
     [:button.view-control__button
      {:class (if (= ::view/solution @view) "view-control__button--active" "")
       :on-click (fn [] (state/edit! doc view/set-map-view ::view/solution))}
      "Solution"]
     (when (= ::view/solution @view)
       [:label.checkbox-label {:for "display-pipe-diams" :style {:margin-left :10px}}
        [:input.checkbox
         {:type :checkbox
          :id "display-pipe-diams"
          :checked (boolean @show-pipe-diameters?)
          :on-change (fn [] (state/edit! doc view/toggle-show-pipe-diameters))}]
        "Display relative pipe diameters"])
     ]))

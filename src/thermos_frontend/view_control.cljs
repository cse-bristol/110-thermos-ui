(ns thermos-frontend.view-control
  (:require [thermos-specs.view :as view]
            [thermos-frontend.editor-state :as state]))

(defn- component [doc]
  [:div.view-control
   [:h2.view-control__label "Map view:"]
   [:button.view-control__button
    {:class (if (= ::view/constraints (-> @doc ::view/view-state ::view/map-view)) "view-control__button--active" "")
     :on-click (fn [] (state/edit! doc view/set-map-view ::view/constraints))}
    "Constraints"]
   [:button.view-control__button
    {:class (if (= ::view/solution (-> @doc ::view/view-state ::view/map-view)) "view-control__button--active" "")
     :on-click (fn [] (state/edit! doc view/set-map-view ::view/solution))}
    "Solution"]
   ])

(ns thermos-frontend.view-control
  (:require [thermos-specs.view :as view]
            [thermos-frontend.editor-state :as state]
            [reagent.core :as r]
            ))

(defn- component [doc]
  (r/with-let [*map-view (r/cursor doc [::view/view-state ::view/map-view])]
    (let [map-view (or @*map-view ::view/constraints)]
      [:div.view-control
       [:h2.view-control__label "Map view:"]
       [:button.view-control__button
        {:class (if (= ::view/constraints map-view) "view-control__button--active" "")
         :on-click (fn [] (state/edit! doc view/set-map-view ::view/constraints))}
        "Constraints"]
       [:button.view-control__button
        {:class (if (= ::view/solution map-view) "view-control__button--active" "")
         :on-click (fn [] (state/edit! doc view/set-map-view ::view/solution))}
        "Solution"]
       ])))

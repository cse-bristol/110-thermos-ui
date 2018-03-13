(ns thermos-ui.frontend.popover
  (:require [reagent.core :as reagent]
            [thermos-ui.specs.view :as view]))

(defn component
  [document]
  (let [showing (->> @document ::view/view-state ::view/popover ::view/showing)
        content (->> @document ::view/view-state ::view/popover ::view/popover-content)
        source-coords (->> @document ::view/view-state ::view/popover ::view/source-coords)]
    [:div {:style {:position "fixed"
                   :top (if source-coords (str (second source-coords) "px") 0)
                   :left (if source-coords (str (first source-coords) "px") 0)
                   :z-index "2000"
                   :display (if showing "block" "none")}}
     content])
  )

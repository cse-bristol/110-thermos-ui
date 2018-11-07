(ns thermos-frontend.popover
  (:require [reagent.core :as reagent]
            [thermos-specs.view :as view]
            [thermos-frontend.editor-state :as state]))

(defn component
  [document]
  (let [showing (->> @document ::view/view-state ::view/popover ::view/popover-showing)
        content (->> @document ::view/view-state ::view/popover ::view/popover-content)
        source-coords (->> @document ::view/view-state ::view/popover ::view/source-coords)]
    (when showing
      (cond
        (seqable? source-coords)
        [:div {:style {:position :fixed
                       :top (str (second source-coords) "px")
                       :left (str (first source-coords) "px")
                       :z-index "2000"
                       :display :block}}
         content]

        (= :middle source-coords)
        [:div {:style {:position :fixed
                       :top 0
                       :left 0
                       :width :100%
                       :height :100%
                       :z-index "2000"
                       :display :flex
                       }}
         [:div {:style {:margin :auto}} content]]
        )
      )))

(defn- show-popover [state content coords]
  (update-in state [::view/view-state ::view/popover]
             assoc
             ::view/popover-showing true
             ::view/popover-content content
             ::view/source-coords coords))

(defn- hide-popover [state]
  (update-in state [::view/view-state ::view/popover]
             assoc
             ::view/popover-showing false
             ::view/popover-content nil
             ::view/source-coords nil))

(defn open! [document content coords]
  (state/edit! document
               show-popover
               content coords))

(defn close! [document]
  (state/edit! document
               hide-popover))

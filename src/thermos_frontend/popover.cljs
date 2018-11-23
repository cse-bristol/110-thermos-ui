(ns thermos-frontend.popover
  (:require [reagent.core :as reagent]
            [thermos-frontend.editor-state :as state]))

(defonce state (reagent/atom {}))
(defonce focus-element (atom nil))

(defn component
  []
  (let [showing (->> @state ::popover-showing)
        content (->> @state ::popover-content)
        source-coords (->> @state ::source-coords)]
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
  (assoc state
          ::popover-showing true
          ::popover-content content
          ::source-coords coords))

(defn- hide-popover [state]
  (assoc state
          ::popover-showing false
          ::popover-content nil
          ::source-coords nil))

(defn open! [content coords]
  (swap! state show-popover content coords))

(defn close! []
  (swap! state hide-popover)
  (when-let [e @focus-element]
    (.focus e)))

(defn visible? [] (get-in @state ::popover-showing))

(defn set-focus-element! [e]
  (reset! focus-element e))

(ns thermos-ui.frontend.editor
  (:require [reagent.core :as reagent :refer [atom]]
            [thermos-ui.frontend.map :as map]
            [thermos-ui.frontend.editor-state :as state]
            [clojure.pprint :refer [pprint]]
            ))

(enable-console-print!)

(defn on-js-reload [])

(defn component []
  [:div
   [:h1 "State:"]
   [:pre (with-out-str (pprint @state/state))]
   [map/component state/state]])

(reagent/render-component
 [component]
 (js/document.getElementById "app"))

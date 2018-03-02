(ns thermos-ui.frontend.editor
  (:require [reagent.core :as r :refer [atom]]
            [thermos-ui.frontend.map :as map]
            [thermos-ui.frontend.editor-state :as state]
            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.frontend.main-nav :as main-nav]
            [thermos-ui.frontend.network-candidates-panel :as network-candidates-panel]
            [thermos-ui.frontend.selection-info-panel :as selection-info-panel]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as s]
            ))

(enable-console-print!)

(defn map-page []
  [:div
   [main-nav/component
    {:on-save
     ;; TODO associate with org. name and doc. name
     #(state/save-document! "test" "document")
     }
    state/state]
   [:div.layout__container
    [:div.layout__panel.layout__panel--left
     [map/component state/state]]
    [:div.layout__panel.layout__panel--right
     [:div.layout__panel.layout__panel--top
      [network-candidates-panel/component state/state]]
     [:div.layout__panel.layout__panel--bottom
      [selection-info-panel/component state/state]]]]])

(defn on-js-reload [])

(defn mount-root []
  (r/render [map-page] (.getElementById js/document "app")))

(mount-root)

(let [[org-name proj-name version]
      (->>
       (-> js/window.location
           (.-pathname)
           (s/split "/"))
       (remove empty?))]

  (when version
    (state/load-document! org-name proj-name version)))

(ns thermos-ui.frontend.editor
  (:require [reagent.core :as r :refer [atom]]
            [thermos-ui.frontend.map :as map]
            [thermos-ui.urls :as urls]
            [thermos-ui.frontend.editor-state :as state]
            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.frontend.main-nav :as main-nav]
            [thermos-ui.frontend.network-candidates-panel :as network-candidates-panel]
            [thermos-ui.frontend.selection-info-panel :as selection-info-panel]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as s]
            ))

(enable-console-print!)

(defn- url-decode [str]
  (js/decodeURIComponent (s/replace str "+" "%20")))

(let [[org-name proj-name version]
      (->>
       (-> js/window.location
           (.-pathname)
           (s/split "/"))
       (remove empty?))

      proj-name (url-decode proj-name)
      ]

  (defn do-save [name]
    (state/save-document!
     org-name name
     (fn [new-id]
       (js/window.history.pushState
        nil
        name
        (urls/editor org-name name new-id)))))

  (defn map-page []
    [:div
     [main-nav/component
      {:on-save do-save
       :name proj-name}]

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

  (when version
    (state/load-document! org-name proj-name version)))

(ns thermos-ui.frontend.editor
  (:require [reagent.core :as r :refer [atom]]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [thermos-ui.frontend.map :as map]
            [thermos-ui.frontend.editor-state :as state]
            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.frontend.main-nav :as main-nav]
            [thermos-ui.frontend.network-candidates-panel :as network-candidates-panel]
            [thermos-ui.frontend.selection-info-panel :as selection-info-panel]
            [clojure.pprint :refer [pprint]]
            ))

(enable-console-print!)

(defn home-page []
  [:div.container-fluid
   [:h1 "Welcome to the the Thermos prototype"]
   [:a {:href "/org/map/"} "View Map"]
    [:form
     [:div.row
      [:div.form-group
       [:input#orgName {:placeholder "Organisation Name"
                        :type "text"
                        :name "orgName"}]]]]])

(defn map-page []
  [:div
   [main-nav/component state/state]
   [:div.layout__container
    [:div.layout__panel.layout__panel--left
     [map/component state/state]]
    [:div.layout__panel.layout__panel--right
     [:div.layout__panel.layout__panel--top
      [network-candidates-panel/component state/state]]
     [:div.layout__panel.layout__panel--bottom
      [selection-info-panel/component state/state]]]]])

(defonce page (atom #'home-page))

(defn current-page []
   [:div [@page]])

(secretary/defroute "/" []
  (reset! page #'home-page))


(secretary/defroute "/:org/map/" []
  (reset! page #'map-page))

(defn on-js-reload [])

(defn mount-root []
    (r/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))

(init!)

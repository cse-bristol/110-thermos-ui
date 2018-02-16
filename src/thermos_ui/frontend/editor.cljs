(ns thermos-ui.frontend.editor
  (:require [reagent.core :as r :refer [atom]]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [thermos-ui.frontend.map :as map]
            [thermos-ui.frontend.editor-state :as state]
            [thermos-ui.frontend.operations :as operations]
            [clojure.pprint :refer [pprint]]
            ))

(enable-console-print!)

(defn toolbar-component []
  [:div {:width "100%"}
   [:button "Load"]
   [:button "Save"]
   [:button "Run"]])

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

;; @TODO Put this into the appropriate component once it exsits.
(defn test-candidate-inclusion-selector []
  [:div {:style {:margin-top "30px"}}
   "Selections is:"
   [:button {:on-click (fn []
                         (let [selected-candidates-ids (operations/selected-candidates-ids @state/state)]
                           (state/edit! state/state
                                        operations/set-candidates-inclusion
                                        selected-candidates-ids :forbidden)))}
    "Forbidden"]
   [:button {:on-click (fn []
                         (let [selected-candidates-ids (operations/selected-candidates-ids @state/state)]
                           (state/edit! state/state
                                        operations/set-candidates-inclusion
                                        selected-candidates-ids :required)))}
    "Required"]
   [:button {:on-click (fn []
                         (let [selected-candidates-ids (operations/selected-candidates-ids @state/state)]
                           (state/edit! state/state
                                        operations/set-candidates-inclusion
                                        selected-candidates-ids :optional)))}
    "Optional"]])

(defn map-page []
  [:div
   [:h1 "State:"]
   [toolbar-component]
   [map/component state/state]
   [test-candidate-inclusion-selector]])

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

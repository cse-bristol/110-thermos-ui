(ns thermos-ui.frontend.hide-forbidden-control
    (:require
      [reagent.core :as reagent]
      [thermos-ui.frontend.editor-state :as state]
      [thermos-ui.specs.document :as document]
      [thermos-ui.specs.view :as view]
      [thermos-ui.specs.candidate :as candidate]
      [thermos-ui.frontend.operations :as operations]))

(defn component
  [leaflet-map]
  (let [show-forbidden? (operations/showing-forbidden? @state/state)]
    [:div.leaflet-touch.leaflet-bar
     [:input {:type "checkbox"
              :id "hide-forbidden-candidates"
              :style {:display "none"}
              :checked (not show-forbidden?)
              :on-change (fn [e] (state/edit-geometry! state/state operations/toggle-showing-forbidden)
                           )
              }]
     [:label.hide-forbidden-control {:for "hide-forbidden-candidates"
                                     :title (if show-forbidden?
                                              "Hide unconstrained candidates"
                                              "Show unconstrained candidates")}
      [:span.hide-forbidden-icon]]]))

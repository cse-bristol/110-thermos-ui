(ns thermos-ui.frontend.zoom-to-selection-control
  (:require
   [thermos-ui.frontend.editor-state :as state]
   [thermos-ui.specs.candidate :as candidate]
   [thermos-ui.frontend.spatial :as spatial]
   [thermos-ui.frontend.operations :as operations]
   ))

(defn component
  [leaflet-map]
  (let [show-forbidden? (operations/showing-forbidden? @state/state)]
    [:div
     [:button.leaflet-control-button
      {:style {:font-size "30px"}
       :title "Zoom to selection (z)"
       :on-click #(state/edit! state/state spatial/zoom-to-selection)
       }
      "⌖"]]))

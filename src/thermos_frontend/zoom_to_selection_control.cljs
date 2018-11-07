(ns thermos-frontend.zoom-to-selection-control
  (:require
   [thermos-frontend.editor-state :as state]
   [thermos-specs.candidate :as candidate]
   [thermos-frontend.spatial :as spatial]
   [thermos-frontend.operations :as operations]
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

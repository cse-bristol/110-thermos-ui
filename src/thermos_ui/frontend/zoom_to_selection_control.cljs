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
       :title "Zoom to selection"
       :on-click (fn [e]
                   (let [selected-candidates (operations/selected-candidates @state/state)
                         selected-candidates-geometries (map #(::candidate/geometry %) selected-candidates)
                         minX (apply min (map #(:minX (::spatial/bbox %)) selected-candidates))
                         maxX (apply max (map #(:maxX (::spatial/bbox %)) selected-candidates))
                         minY (apply min (map #(:minY (::spatial/bbox %)) selected-candidates))
                         maxY (apply max (map #(:maxY (::spatial/bbox %)) selected-candidates))]
                     (.flyToBounds leaflet-map (clj->js [[minY minX] [maxY maxX]]) #js{:animate false})
                     ))
       }
      "⌖"]]))

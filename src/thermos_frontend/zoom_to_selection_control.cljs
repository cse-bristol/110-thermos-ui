;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.zoom-to-selection-control
  (:require
   [re-frame.core :as rf]
   [thermos-specs.candidate :as candidate]
   [thermos-frontend.spatial :as spatial]
   [thermos-frontend.operations :as operations]
   ))

(defn component
  [leaflet-map]
  [:div
   [:button.leaflet-control-button
    {:style {:font-size "30px"}
     :title "Zoom to selection (z)"
     :on-click #(rf/dispatch [:doc/edit spatial/zoom-to-selection])
     }
    "⌖"]])

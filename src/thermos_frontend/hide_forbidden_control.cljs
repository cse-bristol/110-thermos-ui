;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.hide-forbidden-control
    (:require
      [reagent.core :as reagent]
      [re-frame.core :as rf]
      [thermos-specs.document :as document]
      [thermos-specs.view :as view]
      [thermos-specs.candidate :as candidate]
      [thermos-frontend.operations :as operations]))

(defn component
  [leaflet-map]
  (let [show-forbidden? @(rf/subscribe [:map/showing-forbidden])]
    [:div
     [:input {:type "checkbox"
              :id "hide-forbidden-candidates"
              :style {:display "none"}
              :checked (not show-forbidden?)
              :on-change #(rf/dispatch [:doc/edit-geometry operations/toggle-showing-forbidden])
              }]
     [:label.leaflet-control-button.hide-forbidden-control
      {:for "hide-forbidden-candidates"
       :title (if show-forbidden?
                "Hide unconstrained candidates"
                "Show unconstrained candidates")}
      [:span.hide-forbidden-icon]]]))

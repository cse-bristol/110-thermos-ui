;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.view-control
  (:require [reagent.core :as reagent]
            [re-frame.core :as rf]
            [thermos-specs.view :as view]
            [thermos-frontend.theme :as theme]))

(defn component []
  (reagent/with-let [legend-visible? (reagent/atom false)]
    (let [view                 @(rf/subscribe [:view/map-view])
          show-pipe-diameters? @(rf/subscribe [:view/show-pipe-diameters])]
      [:div.view-control
       [:h2.view-control__label "Map view:"]
       [:button.view-control__button
        {:class (if (= ::view/constraints view) "view-control__button--active" "")
         :on-click #(rf/dispatch [:doc/edit view/set-map-view ::view/constraints])}
        "Constraints"]
       [:button.view-control__button
        {:class (if (= ::view/solution view) "view-control__button--active" "")
         :on-click #(rf/dispatch [:doc/edit view/set-map-view ::view/solution])}
        "Solution"]
       (when (= ::view/solution view)
         [:label.checkbox-label {:for "display-pipe-diams" :style {:margin-left :10px}}
          [:input.checkbox
           {:type :checkbox
            :id "display-pipe-diams"
            :checked (boolean show-pipe-diameters?)
            :on-change #(rf/dispatch [:doc/edit view/toggle-show-pipe-diameters])}]
          "Show pipe sizes"])
       [:span.pull-right
        [:button.view-control__button
         {:class (when @legend-visible? "view-control__button--active")
          :on-click #(swap! legend-visible? not)}
         "Map legend"]
        (when @legend-visible?
          [:div.map-legend
           (if (= ::view/constraints view)
             [:dl
              [:div [:dt.key.required-key] [:dd "Required"]]
              [:div [:dt.key.optional-key] [:dd "Optional"]]
              [:div [:dt.key.forbidden-key] [:dd "Forbidden"]]
              [:div [:dt.key.supply-key] [:dd "Network supply"]]]
             ;; Solution
             [:dl
              [:div [:dt.key.in-network-key]
               [:dd "In network"
                (when show-pipe-diameters?
                  (let [[smallest largest] @(rf/subscribe [:solution/diameter-range])]
                    [:div.pipe-size-key
                     [:span.pipe-size-key__min]
                     (Math/round smallest) "mm"
                     [:span.pipe-size-key__max]
                     (Math/round largest) "mm"]))]]
              [:div [:dt.key.not-in-network-key] [:dd "Not in network"]]
              [:div [:dt.key.peripheral-key] [:dd "Peripheral to network"]]
              [:div [:dt.key.unreachable-key] [:dd "Cannot be reached by network"]]
              [:div [:dt.key.alternative-key] [:dd "Has alternative"]]
              [:div [:dt.key.supply-key] [:dd "Network supply"]]])])]
       ])))

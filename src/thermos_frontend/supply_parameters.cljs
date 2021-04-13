;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.supply-parameters
  (:require [thermos-specs.supply :as supply]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.document :as document]
            [thermos-frontend.popover :as popover]
            [thermos-frontend.inputs :as inputs]
            [thermos-frontend.editor-state :as state]
            [reagent.core :as reagent]))

(defn- just-number-values [m] (into {} (filter (comp number? second) m)))

(defn- merge-supply-parameters [document candidate-ids parameters]
  (let [emissions (just-number-values (::supply/emissions parameters))
        parmeters (just-number-values parameters)]
    (document/map-candidates
     document
     #(-> %
          (assoc ::candidate/modified true)
          (update ::supply/emissions merge emissions)
          (merge parameters))
     candidate-ids)))

(defn- supply-parameters-form [document candidate-ids]
  ;; take existing values from document
  (reagent/with-let [candidates (map (::document/candidates @document) candidate-ids)

                     supply-keys [::supply/capacity-kwp
                                  ::supply/cost-per-kwh
                                  ::supply/capex-per-kwp
                                  ::supply/opex-per-kwp
                                  ::supply/fixed-cost
                                  ::supply/emissions]

                     initial-values
                     (into {} (for [k supply-keys] [k (first (map k candidates))]))

                     initial-values (assoc (just-number-values initial-values)
                                           ::supply/emissions (just-number-values (::supply/emissions initial-values)))

                     local-state (reagent/atom initial-values)

                     capacity-kwp (reagent/cursor local-state [::supply/capacity-kwp])
                     cost-per-kwh (reagent/cursor local-state [::supply/cost-per-kwh])
                     capex-per-kwp (reagent/cursor local-state [::supply/capex-per-kwp])
                     opex-per-kwp (reagent/cursor local-state [::supply/opex-per-kwp])
                     fixed-cost (reagent/cursor local-state [::supply/fixed-cost])
                     emissions-atoms (into {}
                                           (for [e candidate/emissions-types]
                                             [e (reagent/cursor local-state [::supply/emissions e])]))
                     ]
    [:div.popover-dialog
     [:div
      [:table
       [:tbody
        [:tr [:td {:col-span 3} [:b "Cost and capacity"]] ]
        ;; TODO check these thresholds are sane
        [:tr [:td "Maximum capacity"] [:td [inputs/number {:value-atom capacity-kwp  :min 1 :max 1000  :scale (/ 1.0 1000)  :step 0.1}]] [:td "MW"]]
        [:tr [:td "Fixed cost"]       [:td [inputs/number {:value-atom fixed-cost    :min 0 :max 10000 :scale (/ 1.0 1000)  :step 0.1}]] [:td "k¤"]]
        [:tr [:td "Capacity cost"]    [:td [inputs/number {:value-atom capex-per-kwp :min 0 :max 10000  :step 0.1}]] [:td "¤/kW"]]
        [:tr [:td "Annual cost"]      [:td [inputs/number {:value-atom opex-per-kwp  :min 0 :max 1000                       :step 0.1}]] [:td "¤/kW"]]
        [:tr [:td "Supply cost"]      [:td [inputs/number {:value-atom cost-per-kwh  :min 0 :max 500   :scale 100           :step 0.1}]] [:td "c/kWh"]]
        [:tr [:td {:col-span 3} [:b "Emissions factors"]] ]
        (for [e candidate/emissions-types]
          [:tr {:key e}
           [:td (name e)]
           [:td [inputs/number {:value-atom (emissions-atoms e) :min 0 :max 1000 :step 1
                                :scale (candidate/emissions-factor-scales e)}]]
           [:td (candidate/emissions-factor-units e)]])]
       ]
      ]
     [:div
      [:button.button.button--danger {:on-click #(popover/close!)}
       "Cancel"]
      [:button.button {:on-click (fn []
                                   (state/edit! document merge-supply-parameters candidate-ids @local-state)
                                   (popover/close!)
                                   )}
       "OK"]]
    ])
  )

(defn show-editor! [document candidate-ids]
  (when (seq candidate-ids)
    (popover/open! [supply-parameters-form document candidate-ids]
                   :middle)))

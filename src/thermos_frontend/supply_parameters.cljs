(ns thermos-frontend.supply-parameters
  (:require [thermos-specs.supply :as supply]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.document :as document]
            [thermos-frontend.popover :as popover]
            [thermos-frontend.inputs :as inputs]
            [thermos-frontend.editor-state :as state]
            [reagent.core :as reagent]
            ))

(defn- just-number-values [m] (into {} (filter (comp number? second) m)))

(defn- merge-supply-parameters [document candidate-ids parameters]
  (let [parameters (just-number-values parameters)]
    (document/map-candidates
     document
     #(merge % parameters)
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
       [:tr [:td {:colspan 3} [:b "Cost and capacity"]] ]
       ;; TODO check these thresholds are sane
       [:tr [:td "Maximum capacity"] [:td [inputs/number {:value-atom capacity-kwp :scale (/ 1.0 1000) :min 1 :max 1000}]]   [:td "MW"]]
       [:tr [:td "Fixed cost"]       [:td [inputs/number {:value-atom fixed-cost :min 0 :max 10000 :scale (/ 1.0 1000)}]]    [:td "k¤"]]
       [:tr [:td "Capacity cost"]    [:td [inputs/number {:value-atom capex-per-kwp :min 0 :max 10000 :scale (/ 1.0 1000)}]] [:td "k¤/kW"]]
       [:tr [:td "Operating cost"]   [:td [inputs/number {:value-atom opex-per-kwp :min 0 :max 10000 :scale (/ 1.0 1000)}]]  [:td "k¤/kW"]]
       [:tr [:td "Supply cost"]      [:td [inputs/number {:value-atom cost-per-kwh :scale 100}]]  [:td "c/kWh"]]
       [:tr [:td {:colspan 3} [:b "Emissions factors"]] ]
       (for [e candidate/emissions-types]
        [:tr {:key e}
         [:td (name e)]
         [:td [inputs/number {:value-atom (emissions-atoms e)
                              :scale 1000 :minimum 0 :maximum 1000}]]
         [:td "g/kWh"]
         ])
       ]
      ]
     [:div
      [:button {:on-click #(popover/close! document)}
       "Cancel"]
      [:button {:on-click (fn []
                            (state/edit! document merge-supply-parameters candidate-ids @local-state)
                            (popover/close! document)
                            )}
       "OK"]]
    ])
  )

(defn show-editor! [document candidate-ids]
  (when (seq candidate-ids)
    (popover/open! document
                   [supply-parameters-form document candidate-ids]
                   :middle)))


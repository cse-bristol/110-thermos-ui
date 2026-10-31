;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.params.supply-objective
  (:require [thermos-frontend.inputs :as inputs]
            [thermos-frontend.doc :as doc]
            [thermos-specs.supply :as supply]
            [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [thermos-frontend.preload :as preload]))

(defn supply-objective-parameters []
  (let [objective #(vector ::supply/objective %)]
    [:div
     [:div.card.flex-grow.parameters-component
      [:h1 "Accounting period"]
      [:p "Sum costs and benefits over "
       [inputs/number {:path (objective :accounting-period) :max 100 :min 1 :step 1}] " years. Discount future values at " [inputs/number {:path (objective :discount-rate) :max 100 :min 0 :scale 100 :step 0.1}] "% per year."]
      [:h1 "Model options"]
      [:p.has-tt
       {:title "This is the financial penalty incurred for each kWh of demand that is unmet."}
       "Curtailment cost: " [inputs/number {:min 0 :max 10000 :step 1 :path (objective :curtailment-cost)}] "¤/kWh"

       ]
      [:p.has-tt {:title "If set, CHP engines will be allowed to produce excess heat in order to sell power."}
       [inputs/check {:label "Allow heat dumping?"
                      :value (doc/value (objective :can-dump-heat))
                      :on-change #(doc/set-value! (objective :can-dump-heat) %)
                      }]]

      [:h1 "Emissions costs"]
      [:p "These are shared with the network emissions costs, for consistency."]
      [:table
       [:thead
        [:tr
         [:th "Emission"]
         [:th "Cost/t"]]]
       [:tbody
        (for [e candidate/emissions-types]
          [:tr {:key e}
           [:td (name e)]
           [:td [inputs/number {:path [::document/emissions-cost e]
                                :min 0
                                :max 1000
                                :scale 1000
                                :step 0.01}]]])]]

      ;; Link to the other page.
      [:h1 "Computing resources"]
      ;; Custom for this page
      [:p "Stop after " [inputs/number {:min 0.1
                                        :max (or (preload/get-value :max-restricted-project-runtime) 100)
                                        :step 0.1
                                        :path (objective :time-limit)}]
       " hours, or when within "
       [inputs/number {:min 0 :max 100 :step 0.5 :scale 100 :path (objective :mip-gap)}]
       "% of the optimum."]
      (when-let [max-project-runtime (:max-project-runtime (preload/get-value :restriction-info))]
        [:p "As this is a restricted project, maximum runtime cannot be above "
         (str max-project-runtime) " hour(s). Any higher values will be ignored."])
      ]]))

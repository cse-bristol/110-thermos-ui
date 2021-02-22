(ns thermos-frontend.params.supply-objective
  (:require [thermos-frontend.inputs :as inputs]
            [reagent.core :as reagent]
            [thermos-specs.supply :as supply]
            [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [thermos-frontend.preload :as preload]))

(defn supply-objective-parameters [doc]
  (reagent/with-let [accounting-period (reagent/cursor doc [::supply/objective :accounting-period])
                     discount-rate     (reagent/cursor doc [::supply/objective :discount-rate])
                     curtailment-cost  (reagent/cursor doc [::supply/objective :curtailment-cost])
                     heat-dumping      (reagent/cursor doc [::supply/objective :can-dump-heat])

                     mip-gap           (reagent/cursor doc [::supply/objective :mip-gap])
                     time-limit        (reagent/cursor doc [::supply/objective :time-limit])

                     emissions-cost (into {} (for [e candidate/emissions-types]
                                               [e (reagent/cursor doc [::document/emissions-cost e])]))
                     ]
    [:div
     [:div.card.flex-grow.parameters-component
      [:h1 "Accounting period"]
      [:p "Sum costs and benefits over "
       [inputs/number {:value-atom accounting-period :max 100 :min 1 :step 1}] " years. Discount future values at " [inputs/number {:value-atom discount-rate :max 100 :min 0 :scale 100 :step 0.1}] "% per year."]
      [:h1 "Model options"]
      [:p.has-tt
       {:title "This is the financial penalty incurred for each kWh of demand that is unmet."}
       "Curtailment cost: " [inputs/number {:min 0 :max 10000 :step 1 :value-atom curtailment-cost}] "¤/kWh"
       
       ]
      [:p.has-tt {:title "If set, CHP engines will be allowed to produce excess heat in order to sell power."}
       [inputs/check {:label "Allow heat dumping?"
                      :value @heat-dumping
                      :on-change #(reset! heat-dumping %)
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
           [:td [inputs/number {:value-atom (emissions-cost e)
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
                                        :value-atom time-limit}]
       " hours, or when within "
       [inputs/number {:min 0 :max 100 :step 0.5 :scale 100 :value-atom mip-gap}]
       "% of the optimum."]
      (when-let [max-restricted-project-runtime (preload/get-value :max-restricted-project-runtime)]
        [:p "As this is a restricted project, maximum runtime cannot be above " 
         (str max-restricted-project-runtime) " hour(s)."])
      ]])

  )

(ns thermos-frontend.model-parameters
  (:require [reagent.core :as reagent]
            [thermos-specs.demand :as demand]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.document :as document]
            [thermos-frontend.inputs :as inputs]))

;; page for editing the big top-level params

;; we need to be able to set

;; loan rate & term
;; NPV rage & term
;; emissions factor
;; MIP gap
;; heat sale price (should this be per connection anyway? do we want a global default?)
;; actually let's make this / connection

(defn parameter-editor [document]
  (reagent/with-let
    [heat-price (reagent/cursor document [::demand/price])
     emissions-factor (into {} (for [e candidate/emissions-types] [e (reagent/cursor document [::demand/emissions e])]))
     emissions-cost (into {} (for [e candidate/emissions-types] [e (reagent/cursor document [::document/emissions-cost e])]))
     emissions-limit (into {} (for [e candidate/emissions-types] [e (reagent/cursor document [::document/emissions-limit :value e])]))
     emissions-check (into {} (for [e candidate/emissions-types] [e (reagent/cursor document [::document/emissions-limit :enabled e])]))

     mip-gap (reagent/cursor document [::document/mip-gap])
     runtime (reagent/cursor document [::document/maximum-runtime])

     npv-term (reagent/cursor document [::document/npv-term])
     npv-rate (reagent/cursor document [::document/npv-rate])

     loan-term (reagent/cursor document [::document/loan-term])
     loan-rate (reagent/cursor document [::document/loan-rate])

     max-pipe-kwp (reagent/cursor document [::document/maximum-pipe-kwp])

     civil-exponent (reagent/cursor document [::document/civil-cost-exponent])
     mechanical-exponent (reagent/cursor document [::document/mechanical-cost-exponent])
     mechanical-fixed (reagent/cursor document [::document/mechanical-cost-per-m])
     mechanical-variable (reagent/cursor document [::document/mechanical-cost-per-m2])

     flow-temperature (reagent/cursor document [::document/flow-temperature])
     return-temperature (reagent/cursor document [::document/return-temperature])
     ground-temperature (reagent/cursor document [::document/ground-temperature])
     ]
    
    [:div.parameters-component
     [:div
      [:h1 "Costs and limits"]
      [:p "These parameters apply to the whole optimisation"]
      [:h2 "Finance and NPV"]

      [:p "Account for operations over "
       [inputs/number {:value-atom npv-term :min 0 :max 100 :step 1}]
       " years, at a discount rate of "
       [inputs/number {:value-atom npv-rate :min 0 :max 100 :step 0.1 :scale 100}]
       "%"]
      [:p "Finance capital over "
       [inputs/number {:value-atom loan-term :min 0 :max 100 :step 1}]
       " years, at an interest rate of "
       [inputs/number {:value-atom loan-rate :min 0 :max 100 :step 0.1 :scale 100}] "%"]
      
      [:h2 "Emissions"]
      [:table
       [:thead
        [:tr [:th "Emission"] [:th "Cost (¤/t)"] [:th "Limit (t)"]]]
       [:tbody
        (for [e candidate/emissions-types]
          [:tr {:key e}
           [:td (name e)]
           [:td [inputs/number {:value-atom (emissions-cost e)
                                :min 0
                                :max 1000
                                :scale 1000
                                :step 0.01}]]
           
           [:td [inputs/check-number {:value-atom (emissions-limit e)
                                      :check-atom (emissions-check e)
                                      :min 0
                                      :scale 1000
                                      :max 10000
                                      :step 1}]]]
          )]
       ]

      [:h2 "Pipes"]
      [:p "Limit pipe capacity to at most "
       [inputs/number {:value-atom max-pipe-kwp :min 0 :max 500 :step 1 :scale (/ 1 1000.0)}]
       " MWp"]
      [:p "Calculate mechanical engineering costs as "
       [inputs/number {:value-atom mechanical-fixed :min 0 :max 5000 :step 1}]
       "¤/m + ("
       [inputs/number {:value-atom mechanical-variable :min 0 :max 5000 :step 1}]
       "× ⌀/m)"
       [:sup [inputs/number {:value-atom mechanical-exponent :min 0 :max 3 :step 0.01}]]
       "/m"
       ]

      [:p "Calculate civil engineering costs as "
       [:em "A"]
       "¤/m + ("
       [:em "B"]
       "× ⌀/m)"
       [:sup [inputs/number {:value-atom civil-exponent :min 0 :max 3 :step 0.01}]]
       "/m"]

      [:h2 "Temperatures"]
      [:p "Use a flow temperature of "
       [inputs/number {:value-atom flow-temperature :min 0 :max 100 :step 1}]
       "°C"
       ]
      
      [:p "Use a return temperature of "
       [inputs/number {:value-atom return-temperature :min 0 :max 100 :step 1}]
       "°C"
       ]

      [:p "Use an average ground temperature of "
       [inputs/number {:value-atom ground-temperature :min 0 :max 20 :step 1}]
       "°C"
       ]
      ]
    [:div
      [:h1 "Site defaults"]
      [:p "These values will be used for sites where you have not input values"]
      [:h2 "Heat sale price"]
      [inputs/number
       {:value-atom heat-price
        :min 0
        :max 100
        :scale 100
        :step 0.1}] "c/kWh"
      [:h2 "Emissions factors"]
      [:table
       [:tbody
        (for [e candidate/emissions-types]
          [:tr {:key e}
           [:td (name e)]
           [:td [inputs/number {:value-atom (emissions-factor e)
                                :min 0
                                :max 1000
                                :step 0.01
                                }]]
           [:td "kg/kWh"]]
          
          )]
       ]
      ]
     [:div
      [:h1 "Optimisation parameters"]
      [:p "These values control the behaviour of the optimiser"]
      [:p
       "Allowable distance from best possible answer "
       [inputs/number {:value-atom mip-gap :min 0 :max 100 :scale 100}] "%"]
      [:p
       "Maximum runtime "
       [inputs/number {:value-atom runtime :min 0 :max 50 :step 0.1}] "h"]
      ]])

  )

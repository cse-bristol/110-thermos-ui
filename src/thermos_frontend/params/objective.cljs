;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.params.objective
  (:require [reagent.core :as reagent]
            [thermos-frontend.inputs :as inputs]
            [thermos-specs.document :as document]
            [thermos-util.finance :as finance]
            [thermos-frontend.format :as format]
            [thermos-specs.candidate :as candidate]
            [thermos-frontend.preload :as preload]))

(defn- capital-cost [doc params name]
  [:tr
   [:td name]
   
   [:td [inputs/check {:value (:annualize @params)
                       :on-change #(swap! params assoc :annualize %)}]]
   [:td [inputs/check {:value (:recur @params)
                       :on-change #(swap! params assoc :recur %)}]]
   [:td [inputs/number {:value-atom (reagent/cursor params [:period])
                        :disabled (not (or (:recur @params)
                                           (:annualize @params)))}]]
   [:td [inputs/number {:value-atom (reagent/cursor params [:rate])
                        :scale 100
                        :step 0.1
                        :disabled (not (:annualize @params))}]]
   [:td
    (->>
     (finance/objective-capex-value (assoc @doc ::document/npv-rate 0) @params 100 false)
     (:present)
     (format/si-number))]
   [:td
    (->> 
     (finance/objective-capex-value @doc @params 100 false)
     (:present)
     (format/si-number))]])


(defn objective-parameters [document]
  (reagent/with-let
    [npv-term (reagent/cursor document [::document/npv-term])
     npv-rate (reagent/cursor document [::document/npv-rate])
     pipework-capex (reagent/cursor document [::document/capital-costs :pipework])
     supply-capex (reagent/cursor document [::document/capital-costs :supply])
     connection-capex (reagent/cursor document [::document/capital-costs :connection])
     insulation-capex (reagent/cursor document [::document/capital-costs :insulation])
     alternative-capex (reagent/cursor document [::document/capital-costs :alternative])
     maximum-supply-sites (reagent/cursor document [::document/maximum-supply-sites])

     emissions-cost (into {} (for [e candidate/emissions-types]
                               [e (reagent/cursor document [::document/emissions-cost e])]))
     emissions-limit (into {} (for [e candidate/emissions-types]
                                [e (reagent/cursor document [::document/emissions-limit :value e])]))
     emissions-check (into {} (for [e candidate/emissions-types]
                                [e (reagent/cursor document [::document/emissions-limit :enabled e])]))

     objective (reagent/cursor document [::document/objective])

     consider-insulation (reagent/cursor document [::document/consider-insulation])
     consider-alternatives (reagent/cursor document [::document/consider-alternatives])

     mip-gap (reagent/cursor document [::document/mip-gap])
     param-gap (reagent/cursor document [::document/param-gap])
     iteration-limit (reagent/cursor document [::document/maximum-iterations])
     runtime (reagent/cursor document [::document/maximum-runtime])

     solver (reagent/cursor document [::document/solver])

     objective-scale (reagent/cursor document [::document/objective-scale])
     objective-precision (reagent/cursor document [::document/objective-precision])
     edge-cost-precision (reagent/cursor document [::document/edge-cost-precision])
     
     ]
    [:div.parameters-component
     [:div.card
      [:h1 "Objective"]
      [:div {:style {:margin-top :1em}}
       [:div {:style {:margin-right :2em}}

        [:div
         [:div
          [:label {:style {:font-size :1.5em}}
           [:input {:type :radio
                    :checked (= :network @objective)
                    :on-change #(reset! objective :network)
                    :value "objective-group"
                    }] "Maximize network NPV"]
          [:div {:style {:margin-left :2em}}
           [:p
            "In this mode, the goal is to choose which demands to connect to the network so as to maximize the NPV " [:em "for the network operator"]
            ". This is the sum of the revenues from demands minus the sum of costs for the network."]
           [:p
            "The impact of non-network factors (individual systems, insulation, and emissions costs) can be accounted for using the " [:em "market"] " tariff, which chooses a price to beat the best non-network system."]]]
         
         (let [is-system-mode (= :system @objective)]
           [:div
            [:label {:style {:font-size :1.5em}}
             [:input {:type :radio
                      :value "objective-group"
                      :checked is-system-mode
                      :on-change #(reset! objective :system)
                      }]
             "Maximize whole-system NPV"]
            [:div {:style {:margin-left :2em}}
             [:p "In this mode, the goal is to choose how to " [:em "supply heat"] " to the buildings in the problem (or abate demand) at the " [:em "minimum overall cost"] ". The internal transfer of money between buildings and network operator is not considered, so there are no network revenues and tariffs have no effect."]
             
             [:div {:style {:display :flex}}
              [:p {:style {:margin-right :1em}}
               [inputs/check {:value @consider-insulation
                              :disabled (not is-system-mode)
                              :on-change #(reset! consider-insulation %)
                              :label "Offer insulation measures"}]]
              [:p {:style {:flex 1}}
               [inputs/check
                {:value @consider-alternatives
                 :disabled (not is-system-mode)
                 :on-change #(reset! consider-alternatives %)
                 :label [:span.has-tt
                         {:title "If checked, buildings have a choice of network, individual systems or sticking with their counterfactual system. Otherwise, the choice is just between network and counterfactual."
                          }
                         "Offer other heating systems"]}]]]]
            ])
         ]
        ]
       
       ]
      [:h1 "Accounting period"]
      [:p
       "Sum costs and benefits over " [inputs/number {:value-atom npv-term
                                                      :step 1
                                                      :min 0
                                                      :max 100}]
       " years. "
       "Discount future values at " [inputs/number {:value-atom npv-rate
                                                    :scale 100
                                                    :min 0
                                                    :max 100
                                                    :step 0.1}] "% per year."]
      ]
     
     [:div {:style {:display :flex :flex-wrap :wrap}}
      [:div.card {:style {:flex-grow 2}}
       [:h1 "Capital costs"]
       [:table
        [:thead
         [:tr
          [:th "Item"]
          [:th.has-tt
           {:title "If checked, the capital cost will be split into a series of equal sized repayments against a loan at the given rate, over the given period."}
           "Annualize"]
          [:th.has-tt
           {:title (str "If checked, the capital cost must be paid again at the end of each period, until the end of the whole accounting period. "
                        "This represents equipment that must be replaced every periodically.")}
           "Recur"]
          [:th "Period"]
          [:th "Rate"]
          [:th.has-tt
           {:title "This is the total cost of 100 currency, over the accounting period."}
           "¤ 100"]
          [:th.has-tt
           {:title "This is the total cost of 100 currency, over the accounting period, with the discount rate applied."}
           "PV(¤ 100)"]]]
        [:tbody
         [capital-cost document pipework-capex "Pipework"]
         [capital-cost document supply-capex "Supply"]
         [capital-cost document connection-capex "Connections"]
         [capital-cost document insulation-capex "Insulation"]
         [capital-cost document alternative-capex "Other heating"]]]]
      
      [:div.card {:style {:flex-grow 1}}
       [:h1 "Emissions costs"]
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
                                 :step 0.01}]]])]]]

      [:div.card {:style {:flex-grow 1}}
       [:h1 "Emissions limits"]
       [:table
        [:thead
         [:tr
          [:th "Emission"]
          [:th "Limited"]
          [:th "Limit (t/yr)"]]]
        [:tbody
         (doall
          (for [e candidate/emissions-types]
            [:tr {:key e}
             [:td (name e)]
             [:td [inputs/check {:value @(emissions-check e)
                                 :on-change #(reset! (emissions-check e) %)}]]
             [:td [inputs/number {:value-atom (emissions-limit e)
                                  :disabled (not @(emissions-check e))
                                  :min 0
                                  :max 10000
                                  :scale 0.001
                                  :step 0.1}]]
             
             ]))]]]

      [:div.card {:style {:flex-grow 1}}
       [:h1 "Supply limit"]
       [:p "Limit the number of supply locations the model can build to: "]

       [:div [inputs/check {:value (boolean @maximum-supply-sites)
                            :on-change
                            #(if %
                               (reset! maximum-supply-sites 1)
                               (reset! maximum-supply-sites nil))
                            }]
        [inputs/number {:value-atom maximum-supply-sites
                        :disabled (not (boolean @maximum-supply-sites))
                        :min 1
                        :max 100
                        :step 1}
         ]]
       ]

      [:div.card {:style {:flex-grow 1}}
       [:h1 "Computing resources"]

       [:p
        "Stop if solution is known to be at least this close to the optimum "
        [inputs/number {:value-atom mip-gap :step 0.1 :min 0 :max 100 :scale 100}] "%"]
       [:p
        "Stop if parameter fixing affects the objective by less than or equal to "
        [inputs/number {:value-atom param-gap :step 0.01 :min 0 :max 100 :scale 100}] "%"]
       [:p
        "Stop after this many iterations "
        [inputs/number {:value-atom iteration-limit :step 1 :min 1 :max 1000
                        :empty-value [nil "1000"]}]]
       [:p
        "Maximum runtime "
        [inputs/number {:value-atom runtime
                        :min 0
                        :max (or (preload/get-value :max-restricted-project-runtime) 50)
                        :step 0.1}] "h"]
       (when-let [max-project-runtime (:max-project-runtime (preload/get-value :restriction-info))]
         [:p "As this is a restricted project, maximum runtime cannot be above "
          (str max-project-runtime) " hour(s). Any higher values will be ignored."])]

      [:div.card {:style {:flex-grow 1}}
       [:h1 "Formulation"]

       [:p
        "Objective scale "
        [inputs/number {:value-atom objective-scale :step 10 :min 1 :max 100 :scale 1}] "¤"
        " — the objective will be scaled down by this amount"]
       
       
       [:p
        "Objective precision "
        [inputs/number {:value-atom objective-precision :step 0.1 :min 0.1 :max 100 :scale 1}] "¤ ✕ scale "
        " — when possible, objective coefficients will be truncated to this many scaled objective units"]
       
       [:p
        "Edge cost precision "
        [inputs/number {:value-atom edge-cost-precision :step 0.1 :min 0.0 :max 100 :scale 100}] "% - edges whose variable cost is below this proportion of their fixed cost will be represented only as a fixed cost."]]
      
      (when (preload/get-value :has-gurobi)
        (let [current-solver (or @solver :scip)]
          [:div.card {:style {:flex-grow 1}}
           [:h1 "Solver"]
           [:p "Use solver: "
            [:label [:input {:type :radio
                             :checked (= :scip current-solver)
                             :on-change #(reset! solver :scip)
                             :value "solver-group"}]
             "SCIP"]
            " "
            [:label [:input {:type :radio
                             :checked (= :gurobi current-solver)
                             :on-change #(reset! solver :gurobi)
                             :value "solver-group"}]
             "Gurobi"]
            ]]))]]))




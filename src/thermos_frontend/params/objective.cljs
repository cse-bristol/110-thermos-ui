;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.params.objective
  (:require [thermos-frontend.doc :as doc]
            [thermos-frontend.inputs :as inputs]
            [thermos-specs.document :as document]
            [thermos-util.finance :as finance]
            [thermos-frontend.format :as format]
            [thermos-specs.candidate :as candidate]
            [thermos-frontend.preload :as preload]))

(defn- capital-cost [path name]
  (let [params (doc/value path)
        npv    {::document/npv-rate (doc/value [::document/npv-rate])
                ::document/npv-term (doc/value [::document/npv-term])}]
    [:tr
     [:td name]

     [:td [inputs/check {:value (:annualize params)
                         :on-change #(doc/set-value! (conj path :annualize) %)}]]
     [:td [inputs/check {:value (:recur params)
                         :on-change #(doc/set-value! (conj path :recur) %)}]]
     [:td [inputs/number {:path (conj path :period)
                          :disabled (not (or (:recur params)
                                             (:annualize params)))}]]
     [:td [inputs/number {:path (conj path :rate)
                          :scale 100
                          :step 0.1
                          :disabled (not (:annualize params))}]]
     [:td
      (->>
       (finance/objective-capex-value (assoc npv ::document/npv-rate 0) params 100 false)
       (:present)
       (format/si-number))]
     [:td
      (->>
       (finance/objective-capex-value npv params 100 false)
       (:present)
       (format/si-number))]]))


(defn objective-parameters []
  (let [objective             (doc/value [::document/objective])
        consider-insulation   (doc/value [::document/consider-insulation])
        consider-alternatives (doc/value [::document/consider-alternatives])
        maximum-supply-sites  (doc/value [::document/maximum-supply-sites])
        solution-method       (doc/value [::document/solution-method])
        solver                (doc/value [::document/solver])
        no-loops              (doc/value [::document/no-loops])]
    [:div.parameters-component
     [:div.card
      [:h1 "Objective"]
      [:div {:style {:margin-top :1em}}
       [:div {:style {:margin-right :2em}}

        [:div
         [:div
          [:label {:style {:font-size :1.5em}}
           [:input {:type :radio
                    :checked (= :network objective)
                    :on-change #(doc/set-value! [::document/objective] :network)
                    :value "objective-group"
                    }] "Maximize network NPV"]
          [:div {:style {:margin-left :2em}}
           [:p
            "In this mode, the goal is to choose which demands to connect to the network so as to maximize the NPV " [:em "for the network operator"]
            ". This is the sum of the revenues from demands minus the sum of costs for the network."]
           [:p
            "The impact of non-network factors (individual systems, insulation, and emissions costs) can be accounted for using the " [:em "market"] " tariff, which chooses a price to beat the best non-network system."]]]
         
         (let [is-system-mode (= :system objective)]
           [:div
            [:label {:style {:font-size :1.5em}}
             [:input {:type :radio
                      :value "objective-group"
                      :checked is-system-mode
                      :on-change #(doc/set-value! [::document/objective] :system)
                      }]
             "Maximize whole-system NPV"]
            [:div {:style {:margin-left :2em}}
             [:p "In this mode, the goal is to choose how to " [:em "supply heat"] " to the buildings in the problem (or abate demand) at the " [:em "minimum overall cost"] ". The internal transfer of money between buildings and network operator is not considered, so there are no network revenues and tariffs have no effect."]
             
             [:div {:style {:display :flex}}
              [:p {:style {:margin-right :1em}}
               [inputs/check {:value consider-insulation
                              :disabled (not is-system-mode)
                              :on-change #(doc/set-value! [::document/consider-insulation] %)
                              :label "Offer insulation measures"}]]
              [:p {:style {:flex 1}}
               [inputs/check
                {:value consider-alternatives
                 :disabled (not is-system-mode)
                 :on-change #(doc/set-value! [::document/consider-alternatives] %)
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
       "Sum costs and benefits over " [inputs/number {:path [::document/npv-term]
                                                      :step 1
                                                      :min 0
                                                      :max 100}]
       " years. "
       "Discount future values at " [inputs/number {:path [::document/npv-rate]
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
         [capital-cost [::document/capital-costs :pipework] "Pipework"]
         [capital-cost [::document/capital-costs :supply] "Supply"]
         [capital-cost [::document/capital-costs :connection] "Connections"]
         [capital-cost [::document/capital-costs :insulation] "Insulation"]
         [capital-cost [::document/capital-costs :alternative] "Other heating"]]]]
      
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
            [:td [inputs/number {:path [::document/emissions-cost e]
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
             [:td [inputs/check {:value (doc/value [::document/emissions-limit :enabled e])
                                 :on-change #(doc/set-value! [::document/emissions-limit :enabled e] %)}]]
             [:td [inputs/number {:path [::document/emissions-limit :value e]
                                  :disabled (not (doc/value [::document/emissions-limit :enabled e]))
                                  :min 0
                                  :max 10000
                                  :scale 0.001
                                  :step 0.1}]]
             
             ]))]]]

      [:div.card {:style {:flex-grow 1}}
       [:h1 "Supply limit"]
       [:p "Limit the number of supply locations the model can build to: "]

       [:div [inputs/check {:value (boolean maximum-supply-sites)
                            :on-change
                            #(if %
                               (doc/set-value! [::document/maximum-supply-sites] 1)
                               (doc/set-value! [::document/maximum-supply-sites] nil))
                            }]
        [inputs/number {:path [::document/maximum-supply-sites]
                        :disabled (not (boolean maximum-supply-sites))
                        :min 1
                        :max 100
                        :step 1}
         ]]
       ]

      [:div.card {:style {:flex-grow 1}}
       [:h1 "Computing resources"]

       (let [method (or solution-method :exact)]
         [:div
          [:p
           [:label [:input {:type :radio
                            :checked (= :exact method)
                            :on-change #(doc/set-value! [::document/solution-method] :exact)
                            :value "solution-method-group"}]
            [:span.has-tt
             {:title "Solve the whole problem as a mixed integer program. This finds the best solution, or one within the gap below, but can be slow for large problems."}
             "Exact"]]
           " "
           [:label [:input {:type :radio
                            :checked (= :automatic method)
                            :on-change #(doc/set-value! [::document/solution-method] :automatic)
                            :value "solution-method-group"}]
            [:span.has-tt
             {:title "Let the optimiser choose how to solve the problem, based on its size and the maximum runtime. Small problems are solved exactly; larger ones use a heuristic and then improve it piece by piece, finishing with an exact solve if there is time."}
             "Automatic"]]]
          (when (and (= :automatic method)
                     (= :system objective)
                     consider-insulation)
            [:p {:style {:color :darkorange}}
             "Insulation is only optimised if the model finishes with an exact solve, which it may not do for large problems."])])

       [:p
        "Stop if solution is known to be at least this close to the optimum "
        [inputs/number {:path [::document/mip-gap] :step 0.1 :min 0 :max 100 :scale 100}] "%"]
       [:p
        "Stop if parameter fixing affects the objective by less than or equal to "
        [inputs/number {:path [::document/param-gap] :step 0.01 :min 0 :max 100 :scale 100}] "%"]
       [:p
        "Stop after this many exact solve iterations "
        [inputs/number {:path [::document/maximum-iterations] :step 1 :min 1 :max 1000
                        :empty-value [nil "1000"]}]]
       [:p
        "Maximum runtime "
        [inputs/number {:path [::document/maximum-runtime]
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
        [inputs/number {:path [::document/objective-scale] :step 10 :min 1 :max 100 :scale 1}] "¤"
        " — the objective will be scaled down by this amount."]
       
       
       [:p
        "Objective precision "
        [inputs/number {:path [::document/objective-precision] :step 0.1 :min 0.1 :max 100 :scale 1}] "¤ ✕ scale "
        " — when possible, objective coefficients will be truncated to this many scaled objective units."]
       
       [:p
        "Edge cost precision "
        [inputs/number {:path [::document/edge-cost-precision] :step 0.1 :min 0.0 :max 100 :scale 100}] "% — edges whose variable cost is below this proportion of their fixed cost will be represented only as a fixed cost."]

       [:p
        [:label [inputs/check
                 {:value (boolean no-loops)
                  :on-change
                  #(if %
                     (doc/set-value! [::document/no-loops] true)
                     (doc/set-value! [::document/no-loops] false))}]
         "Prevent loops - the optimiser will try not to make more than one path through the network to any point."]]]
      
      
      
      (when (preload/get-value :has-gurobi)
        (let [current-solver (or solver :scip)]
          [:div.card {:style {:flex-grow 1}}
           [:h1 "Solver"]
           [:p "Use solver: "
            [:label [:input {:type :radio
                             :checked (= :scip current-solver)
                             :on-change #(doc/set-value! [::document/solver] :scip)
                             :value "solver-group"}]
             "SCIP"]
            " "
            [:label [:input {:type :radio
                             :checked (= :gurobi current-solver)
                             :on-change #(doc/set-value! [::document/solver] :gurobi)
                             :value "solver-group"}]
             "Gurobi"]
            ]]))]]))




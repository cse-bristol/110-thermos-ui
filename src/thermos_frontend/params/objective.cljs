(ns thermos-frontend.params.objective
  (:require [reagent.core :as reagent]
            [thermos-frontend.inputs :as inputs]
            [thermos-specs.document :as document]
            [thermos-util.finance :as finance]
            [thermos-frontend.format :as format]
            [thermos-specs.candidate :as candidate]))

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
    (->> 100
         (finance/objective-capex-value (assoc @doc ::document/npv-rate 0) @params)
         (:present)
         (format/si-number))]
   [:td
    (->> 100
         (finance/objective-capex-value @doc @params)
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
     runtime (reagent/cursor document [::document/maximum-runtime])
     ]
    [:div.parameters-component
     [:div.card
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

      [:h1 "Objective"]
      [:div {:style {:margin-top :1em}}
       [:div {:style {:margin-right :2em}}
        [inputs/radio-group
         {:value @objective
          :on-change #(reset! objective %)
          :options
          [{:key :network :label [:span.has-tt
                                  {:title
                                   (str "This only considers the costs and revenues of the network."
                                        "It includes emissions costs, and if allowed below the capital costs of other systems or insulation."
                                        "It does not include the operating costs of other systems.")}
                                  "Maximize network NPV"]}
           {:key :system :label [:span.has-tt
                                 {:title
                                  "This includes the costs for heating buildings not on the network, and excludes any revenue for the network."}
                                 "Maximize whole-system NPV"]}]}]]
       [:div {:style {:display :flex}}
        [:p {:style {:margin-right :1em}}
         [inputs/check {:value @consider-insulation
                        :on-change #(reset! consider-insulation %)
                        :label "Offer insulation measures"}]]
        [:p {:style {:flex 1}}
         [inputs/check {:value @consider-alternatives
                        :on-change #(reset! consider-alternatives %)
                        :label [:span.has-tt
                                {:title
                                 "If this is not checked, buildings must either connect to the network or use their counterfactual system (if set)."}
                                "Offer other heating systems"]}]]]
       ]]
     
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
                                  :step 1}]]
             
             ]))]]]

      [:div.card {:style {:flex-grow 1}}
       [:h1 "Computing resources"]

       [:p
        "Stop if solution is known to be at least this close to the optimum "
        [inputs/number {:value-atom mip-gap :min 0 :max 100 :scale 100}] "%"]
       [:p
        "Maximum runtime "
        [inputs/number {:value-atom runtime :min 0 :max 50 :step 0.1}] "h"]
       ]]]))



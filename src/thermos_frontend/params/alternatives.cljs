(ns thermos-frontend.params.alternatives
  (:require [reagent.core :as reagent]
            [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.supply :as supply]
            [thermos-pages.symbols :as symbols]
            [thermos-frontend.util :refer [target-value]]
            [thermos-frontend.inputs :as inputs]
            [thermos-util :refer [next-integer-key]]))

(defn- create-new-alternative [alternatives]
  (let [id (next-integer-key alternatives)]
    (assoc alternatives id
           #::supply
           {:id id
            :name ""
            :cost-per-kwh 0
            :capex-per-kwp 0
            :opex-per-kwp 0
            :fixed-cost 0
            :emissions {}})))

(defn- alternative-row [*doc id alternative *alternatives]
  [:div.card.flex-rows
   [:div.flex-cols
    "Name: "[inputs/text
             :style {:flex-grow 1}
             :value (::supply/name alternative "")
             :on-change #(swap! *alternatives assoc-in
                                [id ::supply/name] (target-value %))]]
   [:div.flex-cols
    {:style {:flex-wrap :wrap}}
    [:div
     [:b "Costs"]
     [:div "Heat cost / kWh"
      [inputs/number
       {:min 0 :max 100 :scale 100
        :step 0.1 :value (::supply/cost-per-kwh alternative 0)
        :on-change #(swap! *alternatives assoc-in
                           [id ::supply/cost-per-kwh] %)}]
      "c/kWh"]

     [:div "Fixed capital cost"
      [inputs/number
       {:min 0 :max 50000
        :value (::supply/fixed-cost alternative 0)
        :on-change #(swap! *alternatives assoc-in
                           [id ::supply/fixed-cost] %)}]
      "¤"]

     [:div "Variable capital cost"
      [inputs/number
       {:min 0 :max 1000
        :value (::supply/capex-per-kwp alternative 0)
        :on-change #(swap! *alternatives assoc-in
                           [id ::supply/capex-per-kwp] %)}]
      "¤/kWp"]

     [:div "Operating cost"
      [inputs/number
       {:min 0 :max 1000
        :value (::supply/opex-per-kwp alternative 0)
        :on-change #(swap! *alternatives assoc-in
                           [id ::supply/opex-per-kwp] %)}]
      "¤/kWp"]]

    [:div
     [:b "Emissions factors"]
     [:table
      [:thead
       [:tr
        [:th "Type"]
        [:th "kg/kWh"]]]
      [:tbody
       (for [e candidate/emissions-types]
         [:tr {:key e}
          [:td (name e)]
          [:td [inputs/number
                {:min 0 :max 1000 :step 0.1
                 :value (get-in alternative [::supply/emissions e] 0)
                 :on-change #(swap! *alternatives
                                    assoc-in
                                    [id ::supply/emissions e]
                                    %)}]]])]]]]
   [:button.button
    {:on-click #(swap! *doc document/remove-alternative id)}
    symbols/dustbin]
   ])

(defn alternatives-parameters [doc]
  (reagent/with-let [*alternatives
                     (reagent/cursor doc [::document/alternatives])]
    [:div
     [:div.card.flex-cols
      [:div
       "Buildings can either use a heat network or an individual system as their heat source."
       "Here you can define the parameters of individual systems."]
      
      [:div.flush-right
       [:button.button
        {:on-click #(swap! *alternatives create-new-alternative)}
        symbols/plus " Add"]]]

     (for [[id alternative] @*alternatives]
       [:div {:key id}
        [alternative-row doc id alternative *alternatives]])]))



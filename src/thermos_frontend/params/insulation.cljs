(ns thermos-frontend.params.insulation
  (:require [reagent.core :as reagent]
            [thermos-frontend.inputs :as inputs]
            [thermos-pages.symbols :as symbols]
            [thermos-specs.document :as document]
            [thermos-specs.measure :as measure]
            [thermos-specs.view :as view]
            [thermos-util :refer [next-integer-key]]
            [thermos-frontend.util :refer [target-value]]))

(defn- insulation-row [{id :key} measure *insulation]
  [:div.card {:key id :style {:flex 1}}
   [:label "Name: " [inputs/text
                     :on-change #(swap! *insulation assoc-in [id ::measure/name] (target-value %))
                     :value (::measure/name measure)]]

   [inputs/radio-group {:options [{:key :roof :label "Roof"}
                                  {:key :floor :label "Floor"}
                                  {:key :wall :label "Wall"}]
                        :value (::measure/surface measure)
                        :on-change #(swap! *insulation assoc-in [id ::measure/surface] %)}]
   
   [:div
    [:div
     [:label "Fixed cost: "
      [inputs/number
       {:min 0
        :value (::measure/fixed-cost measure)
        :on-change #(swap! *insulation assoc-in [id ::measure/fixed-cost] %)}]]
     [:label " Cost/m2: "
      [inputs/number
       {:min 0
        :value (::measure/cost-per-m2 measure)
        :on-change #(swap! *insulation assoc-in [id ::measure/cost-per-m2] %)}]]]
    [:div
     [:label "Maximum kWh reduction %: "
      [inputs/number
       {:min 0 :max 100
        :scale 100
        :value (::measure/maximum-effect measure)
        :on-change #(swap! *insulation assoc-in [id ::measure/maximum-effect] %)}]]
     [:label "Maximum area %: "
      [inputs/number
       {:scale 100
        :min 0 :max 100
        :value (::measure/maximum-area measure)
        :on-change #(swap! *insulation assoc-in [id ::measure/maximum-area] %)}]]]]])

(defn- create-new-measure [insulation]
  (let [id (next-integer-key insulation)]
    (assoc insulation id
           {::measure/id id
            ::measure/name ""
            ::measure/surface :wall
            ::measure/fixed-cost 0.0
            ::measure/cost-per-m2 0.0
            ::measure/maximum-effect 0.25})))

(defn insulation-parameters [doc]
  (reagent/with-let [*insulation
                     (reagent/cursor doc [::document/insulation])]
    [:div
     [:div.card.flex-cols
      [:div
       "Buildings can have insulation measures, which reduce their heat demand. "
       "Here you can define insulation measures - each one has:"
       [:ul
        [:li "The type of area it applies to"]
        [:li "A fixed cost and cost per unit area installed"]
        [:li "A maximum reduction - this is a percentage of annual demand that is removed if the measure is fully installed"]
        [:li "A maximum area - this is the percentage of the building's area that is used by installing the measure fully"]]
       "To allow the model to use insulation measures configure the "
       [:a {:href "#"
            :on-click #(swap! doc view/switch-to-tab :parameters)} "objective settings"]
       "."]
      
      [:div.flush-right
       [:button.button
        {:on-click #(swap! *insulation create-new-measure)}
        symbols/plus " Add"]]]

     [:div.flex-cols {:style {:flex-wrap :wrap}}
      (for [[id measure] (sort-by first @*insulation)]
        [insulation-row {:key id} measure *insulation])]]))

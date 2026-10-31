;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.params.insulation
  (:require [re-frame.core :as rf]
            [thermos-frontend.doc :as doc]
            [thermos-frontend.inputs :as inputs]
            [thermos-pages.symbols :as symbols]
            [thermos-specs.document :as document]
            [thermos-specs.measure :as measure]
            [thermos-specs.view :as view]
            [thermos-util :refer [next-integer-key]]))

(defn- insulation-row
  [{id :key} measure]
  (let [path #(vector ::document/insulation id %)]
    [:tr {:key id :style {:height :50px}}
     [:td
      [inputs/text {:path (path ::measure/name)}]]
     [:td
      [inputs/radio-group
       {:options [{:key :roof :label "Roof"}
                  {:key :floor :label "Floor"}
                  {:key :wall :label "Wall"}]
        :value (::measure/surface measure)
        :on-change #(doc/set-value! (path ::measure/surface) %)}]]
     [:td
      [inputs/number2
       {:min 0
        :max 10000
        :style {:max-width :4em}
        :path (path ::measure/fixed-cost)}]
      " ¤"]
     [:td
      [inputs/number2
       {:min 0
        :max 1000
        :style {:max-width :4em}
        :path (path ::measure/cost-per-m2)}]
      " ¤/m" [:sup "2"]]
     [:td
      [inputs/number2
       {:min 0 :max 100
        :style {:max-width :4em}
        :scale 100
        :path (path ::measure/maximum-effect)}]
      " %"]
     [:td
      [inputs/number2
       {:scale 100
        :style {:max-width :4em}
        :min 0 :max 100
        :path (path ::measure/maximum-area)}]
      " %"]
     [:td {:style {:width :1px}}
      [:button.button
       {:title "Delete"
        :on-click #(rf/dispatch [:doc/edit document/remove-insulation id])}
       symbols/cross]]]))

(defn- create-new-measure [insulation]
  (let [id (next-integer-key insulation)]
    (assoc insulation id
           {::measure/id id
            ::measure/name ""
            ::measure/surface :wall
            ::measure/fixed-cost 0.0
            ::measure/cost-per-m2 0.0
            ::measure/maximum-effect 0.25})))

(defn insulation-parameters []
  (let [insulation (doc/value [::document/insulation])]
    [:div
     [:div.card
      [:h2.card-header "Insulation"]
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
            :on-click #(rf/dispatch [:doc/edit view/switch-to-tab :parameters])} "objective settings"]
       "."]

      [:br] [:br]

      (when (seq insulation)
        [:table.table
         [:thead
          [:tr
           [:th "Name"]
           [:th "Applies to"]
           [:th "Fixed cost"]
           [:th "Variable cost"]
           [:th "Maximum Effect"]
           [:th "Maximum area"]
           [:th]]]
         [:tbody
          (for [[id measure] (sort-by first insulation)]
            [insulation-row {:key id} measure])]])
      [:div {:style {:text-align :center :margin-top :10px}}
       [:button.button
        {:on-click #(doc/update! [::document/insulation] create-new-measure)}
        symbols/plus " Add measure"]]]
     ]))

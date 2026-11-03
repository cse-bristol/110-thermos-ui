;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.params.supply-types
  "Types of supply technology, which supply points can take their costs from."
  (:require [re-frame.core :as rf]
            [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.supply :as supply]
            [thermos-pages.symbols :as symbols]
            [thermos-frontend.doc :as doc]
            [thermos-frontend.inputs :as inputs]
            [thermos-frontend.params.cost-curve :refer [cost-curve]]
            [thermos-util :refer [next-integer-key]]))

(defn- create-new-type [types]
  (let [id (next-integer-key types)]
    (assoc types id
           #::supply
           {:id id
            :name ""
            :capacity-kwp 1000.0
            :cost-per-kwh 0
            :capex-per-kwp 0
            :opex-per-kwp 0
            :fixed-cost 0
            :emissions {}})))

(defn capex-editor
  "Edit the capital cost of `supply`, which is either a fixed cost and a
  cost per kW, or a curve. `put` is called with a key and a value,
  where nil means remove the key."
  [supply put]
  (let [curve? (contains? supply ::supply/capex-curve)]
    [:div
     [:div
      [inputs/radio-group
       {:options [{:key :linear :label "Fixed + per kW"}
                  {:key :curve  :label "Cost curve"}]
        :value (if curve? :curve :linear)
        :on-change
        (fn [mode]
          (case mode
            :curve (do (put ::supply/capex-curve (supply/capex-curve supply))
                       (put ::supply/fixed-cost nil)
                       (put ::supply/capex-per-kwp nil))
            :linear (let [[[_ fixed] [x1 y1]] (supply/capex-curve supply)]
                      (put ::supply/capex-curve nil)
                      (put ::supply/fixed-cost (or fixed 0))
                      (put ::supply/capex-per-kwp
                           (if (and x1 (pos? x1)) (/ (- y1 fixed) x1) 0)))))}]]
     (if curve?
       [cost-curve {:value (::supply/capex-curve supply)
                    :max-kw (::supply/capacity-kwp supply)
                    :on-change #(put ::supply/capex-curve %)}]
       [:table
        [:tbody
         [:tr [:td "Fixed capital cost"]
          [:td [inputs/number2 {:min 0 :style {:max-width :7em}
                                :value (::supply/fixed-cost supply 0)
                                :on-change #(put ::supply/fixed-cost %)}]]
          [:td "¤"]]
         [:tr [:td "Capital cost per kW"]
          [:td [inputs/number2 {:min 0 :style {:max-width :7em}
                                :value (::supply/capex-per-kwp supply 0)
                                :on-change #(put ::supply/capex-per-kwp %)}]]
          [:td "¤/kWp"]]]])]))

(defn- type-row [{id :key} tech]
  (let [path (fn [& ks] (into [::document/supply-technologies id] ks))
        put  (fn [k v] (if (nil? v)
                         (doc/update! (path) dissoc k)
                         (doc/set-value! (path k) v)))]
    [:div.crud-list__item {:key id}
     [:div.flex-cols
      [:div.crud-list__title
       [inputs/text {:style {:flex-grow 1}
                     :path (path ::supply/name)
                     :placeholder "Name"}]]
      [:button.crud-list__delete
       {:title "Delete (supply points using this type keep its values)"
        :on-click #(rf/dispatch [:doc/edit document/remove-supply-technology id])}
       symbols/cross]]
     [:div.flex-cols
      [:div.flex-grow
       [:h3 "Capacity and running costs"]
       [:table
        [:tbody
         [:tr [:td "Maximum capacity"]
          [:td [inputs/number2 {:min 0 :scale (/ 1.0 1000) :style {:max-width :6em}
                                :value (::supply/capacity-kwp tech 0)
                                :on-change #(put ::supply/capacity-kwp %)}]]
          [:td "MW"]]
         [:tr [:td "Heat cost"]
          [:td [inputs/number2 {:min 0 :scale 100 :step 0.1 :style {:max-width :6em}
                                :value (::supply/cost-per-kwh tech 0)
                                :on-change #(put ::supply/cost-per-kwh %)}]]
          [:td "c/kWh"]]
         [:tr [:td "Operating cost"]
          [:td [inputs/number2 {:min 0 :style {:max-width :6em}
                                :value (::supply/opex-per-kwp tech 0)
                                :on-change #(put ::supply/opex-per-kwp %)}]]
          [:td "¤/kWp/yr"]]]]
       [:h3 "Emissions factors"]
       [:table
        [:tbody
         (for [e candidate/emissions-types]
           [:tr {:key e}
            [:td (name e)]
            [:td [inputs/number2
                  {:min 0 :style {:max-width :6em}
                   :scale (candidate/emissions-factor-scales e)
                   :value (get-in tech [::supply/emissions e] 0)
                   :on-change #(doc/set-value! (path ::supply/emissions e) %)}]]
            [:td (candidate/emissions-factor-units e)]])]]]
      [:div.flex-grow
       [:h3 "Capital cost"]
       [capex-editor tech put]]]]))

(defn supply-types-parameters []
  (let [types (doc/value [::document/supply-technologies])]
    [:div.card
     [:h2.card-header "Supply types"]
     [:div
      "A supply point can take its capacity and costs from a supply type, "
      "and override some of them with its own values. "
      "Changing a supply type here changes every supply point which uses it."]
     [:br] [:br]
     [:div.crud-list
      (for [[id tech] (sort-by first types)]
        [type-row {:key id} tech])]
     [:div {:style {:text-align :center :margin-top :20px :max-width :900px}}
      [:button.button
       {:on-click #(doc/update! [::document/supply-technologies] create-new-type)}
       symbols/plus " Add supply type"]]]))

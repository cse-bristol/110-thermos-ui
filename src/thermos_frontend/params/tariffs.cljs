;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.params.tariffs
  (:require [reagent.core :as reagent]
            [thermos-frontend.inputs :as inputs]
            [thermos-specs.document :as document]
            [thermos-specs.tariff :as tariff]
            [thermos-frontend.util :refer [target-value]]
            [thermos-pages.symbols :as symbols]
            [clojure.string :as string]))

(defn- vconj [v e]
  (conj (vec v) e))

(def standing-charge-unit "¤/yr")
(def capacity-charge-unit "¤/kWp.yr")
(def unit-rate-unit "c/kWh")


(defn- tariff-row
  [{id :key} *document *tariffs]
  (let [get #(get-in @*tariffs [id %])
        put #(swap! *tariffs assoc-in [id %1] %2)
        delete-tariff #(swap! *document document/remove-tariff id)]
    [:tr {:key id}
     [:td [inputs/text
           {:placeholder (str "Tariff " id)
            :value (get ::tariff/name)
            :on-change #(put ::tariff/name (target-value %))}]]
     [:td [:label
           [inputs/number2
            {:title "A fixed annual payment from customers on this tariff."
             :style {:max-width  :4em}
             :max 1000
             :min 0
             :value (get ::tariff/standing-charge)
             :on-change #(put ::tariff/standing-charge %)
             }]
           " " standing-charge-unit]]
     [:td [:label
           [inputs/number2
            {:title "The heat price paid by customers on this tariff."
             :style {:max-width  :4em}
             :max 100
             :min 0
             :scale 100
             :step 0.1
             :value (get ::tariff/unit-charge)
             :on-change #(put ::tariff/unit-charge %)
             }]
           " " unit-rate-unit]]
     [:td [:label
           [inputs/number2
            {:title "An annual payment per kWp capacity from customers on this tariff."
             :style {:max-width  :4em}
             :max 1000
             :min 0
             :value (get ::tariff/capacity-charge)
             :on-change #(put ::tariff/capacity-charge %)
             }]
           " " capacity-charge-unit]
      ]
     [:td {:style {:width :1px} :title "Delete"}
      [:button.button {:style {:margin-left :1em}
                            :on-click delete-tariff}
            symbols/dustbin]]
     ]))

(defn tariff-parameters
  [doc]
  (reagent/with-let
    [*tariffs          (reagent/cursor doc [::document/tariffs])
     
     *market-rate      (reagent/cursor doc [::tariff/market-discount-rate])
     *market-term      (reagent/cursor doc [::tariff/market-term])
     *market-stick     (reagent/cursor doc [::tariff/market-stickiness])
     *objective        (reagent/cursor doc [::document/objective])
     ]
    [:div
     [:div.card
      [:h2.card-header "Tariffs"]
      [:p "Each building can have an associated tariff, which determines the revenue to the network operator. "]

      (when (= :system @*objective)
        [:p [:b "Note: " ]
         "these settings will have no effect while the objective is whole-system NPV. "
         "Change to network NPV to make network revenue part of the objective."])
      
      (when-not (seq @*tariffs)
        [:p "At the moment you have no tariffs, so connections will produce no revenue or cost. Click 'add' to define a tariff."])

      (when (seq @*tariffs)
        [:table.table {:style {:max-width :900px}}
         [:thead
          [:tr
           [:th "Tariff name"]
           [:th "Standing charge"]
           [:th "Unit charge"]
           [:th "Capacity charge"]
           [:th]]]
         [:tbody
          (doall
            (for [id (sort (keys @*tariffs))]
              [tariff-row {:key id} doc *tariffs]))]])

      [:div.centre {:style {:max-width :900px}}
       [:button.button
        {:style {:margin-top :1em}
         :on-click #(swap! *tariffs
                           (fn [t]
                             (let [id (inc (reduce max -1 (keys t)))]
                               (assoc
                                t
                                id
                                {::tariff/name ""
                                 ::tariff/id id ;; urgh? yes? no?
                                 ::tariff/standing-charge 0
                                 ::tariff/capacity-charge 0
                                 ::tariff/unit-charge 0
                                 }))))}
        symbols/plus " Add tariff"]]

      [:div {:style {:margin-top :2em}}
       (reagent/with-let [more (reagent/atom false)]
         [:div
          [:span "For buildings on the "[:b "market"]" tariff, the unit rate is chosen to beat the building's best individual system. "]
          [:button.button.button--link-style {:on-click #(swap! more not)} (if @more "less..." "more...")]
          (when @more
            [:p "To do this, the building's lowest present cost option is calculated, considering insulation, individual systems, or sticking with the counterfactual. The unit rate is then chosen to give a present cost to the building for connecting to the network which is " [:em "stickiness"] " % less."])
          ]
         )
       [:div {:style {:margin-top :1em}}
        [:table.table.table--variable-width
         [:tbody
          [:tr
           [:td "Discount rate:"]
           [:td
            [inputs/number
             {:title "The discount rate used when evaluating market options"
              :min 0
              :max 10
              :scale 100
              :step 0.1
              :value-atom *market-rate}]
            " %"]]

          [:tr
           [:td "Period:"]
           [:td
            [inputs/number
             {:title "The time period used when evaluating market options"
              :min 0
              :max 50
              :step 1
              :value-atom *market-term}]
            " yr"]]

          [:tr
           [:td "Stickiness: "]
           [:td
            [inputs/number
             {:title "The amount by which the heat network unit rate should try to beat the best individual system."
              :min 0
              :max 100
              :scale 100
              :step 0.5
              :value-atom *market-stick}]
            " %"]]]]]]]



     ]))

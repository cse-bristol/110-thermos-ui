;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.params.alternatives
  (:require [reagent.core :as reagent]
            [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.supply :as supply]
            [thermos-pages.symbols :as symbols]
            [thermos-frontend.util :refer [target-value]]
            [thermos-frontend.inputs :as inputs]
            [thermos-util :refer [next-integer-key]]
            [clojure.string :as string]
            [thermos-util :as util]))

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
            :emissions {}
            :kwp-per-mean-kw nil
            })))

(defn- alternative-row
  [{id :key} *doc alternative *alternatives]
  [:div.crud-list__item {:key id}
   [:div.flex-cols
    [:div.crud-list__title
     [inputs/text
      {:style {:flex-grow 1}
       :value (::supply/name alternative "")
       :placeholder "Name"
       :on-change #(swap! *alternatives assoc-in
                          [id ::supply/name] (target-value %))}]]
     [:button.crud-list__delete
      {:title "Delete"
       :on-click #(swap! *doc document/remove-alternative id)}
      symbols/cross]
    ]
   [:div.flex-cols
    [:div.flex-grow
     [:h3 "Costs"]
     [:table
      [:tbody
       [:tr {:key "heat-cost"}
        [:td "Heat cost / kWh"]
        [:td [inputs/number2
              {:min 0 :max 100 :scale 100
               :style {:max-width :6em}
               :step 0.1 :value (::supply/cost-per-kwh alternative 0)
               :on-change #(swap! *alternatives assoc-in
                                  [id ::supply/cost-per-kwh] %)}]]
        [:td "c/kWh"]
        ]
       [:tr {:key "fixed-cost"}
        [:td "Fixed capital cost"]
        [:td
         [inputs/number2
          {:min 0 :max 50000
           :style {:max-width :6em}
           :value (::supply/fixed-cost alternative 0)
           :on-change #(swap! *alternatives assoc-in
                              [id ::supply/fixed-cost] %)}]]
        [:td
         "¤"]
        ]
       [:tr {:key "var-cost"}
        [:td "Variable capital cost"]
        [:td
         [inputs/number2
          {:min 0 :max 1000
           :style {:max-width :6em}
           :value (::supply/capex-per-kwp alternative 0)
           :on-change #(swap! *alternatives assoc-in
                              [id ::supply/capex-per-kwp] %)}]]
        [:td
         "¤/kWp"]
        ]
       [:tr {:key "op-cost"}
        [:td "Operating cost"]
        [:td
         [inputs/number2
          {:min 0 :max 1000
           :style {:max-width :6em}
           :value (::supply/opex-per-kwp alternative 0)
           :on-change #(swap! *alternatives assoc-in
                              [id ::supply/opex-per-kwp] %)}]]
        [:td
         "¤/kWp"]
        ]
       [:tr {:key "capex per connection"}
        [:td "Capex per Connection"]
        [:td
         [inputs/number2
          {:style {:max-width :6em}
           :value (::supply/capex-per-connection alternative 0)
           :on-change #(swap! *alternatives assoc-in
                              [id ::supply/capex-per-connection] %)}]]
        [:td
         "¤/n"]
        ]
       [:tr {:key "opex per connection"}
        [:td "Opex per Connection"]
        [:td
         [inputs/number2
          {:style {:max-width :6em}
           :value (::supply/opex-per-connection alternative 0)
           :on-change #(swap! *alternatives assoc-in
                              [id ::supply/opex-per-connection] %)}]]
        [:td
         "¤/n"]
        ]
       [:tr
        [:td [:span.has-tt
              {:title "If provided, the peak supplied by this system will be calculated as this multiple of the average heat demand in the year. This models systems with hot water tanks, like heat pumps - a value of 6.5 is reasonable."}
              "Tank factor"]]
        [:td
         [inputs/fmt
          {:min 1 :max 20 :step 0.1
           :style {:max-width :6em}
           :class     ["input" "number-input"]
           :type      :number
           :read      (fn [s] (util/as-double s))
           :print     (fn [v] (if (nil? v) "" v))
           :validate  (constantly nil)
           :on-change #(if %
                         (swap! *alternatives assoc-in [id ::supply/kwp-per-mean-kw] %)
                         (swap! *alternatives update id dissoc ::supply/kwp-per-mean-kw))
           :value     (::supply/kwp-per-mean-kw alternative)
           }
          ]
         ]
        [:td "kWp/kW.avg"]
        ]
       ]]
     ]
    [:div.flex-grow
     [:h3 "Emissions factors"]
     [:table
      [:tbody
       (for [e candidate/emissions-types]
         [:tr {:key e}
          [:td (name e)]
          [:td [inputs/number2
                {:min 0 :max 1000 :step 1
                 :style {:max-width :6em}
                 :scale (candidate/emissions-factor-scales e)
                 :value (get-in alternative [::supply/emissions e] 0)
                 :on-change #(swap! *alternatives
                                    assoc-in
                                    [id ::supply/emissions e]
                                    %)}]]
          [:td (candidate/emissions-factor-units e)]])]]]]])

(defn alternatives-parameters [doc]
  (reagent/with-let [*alternatives
                     (reagent/cursor doc [::document/alternatives])]

    [:div.card
     [:h2.card-header "Individual Systems"]
     [:div
      "Buildings can either use a heat network or an individual system as their heat source. "
      "Here you can define the parameters of individual systems."]
     [:br] [:br]

     [:div.crud-list
      (for [[id alternative] (sort-by first @*alternatives)]
        [alternative-row {:key id} doc alternative *alternatives])]

     [:div {:style {:text-align :center :margin-top :20px :max-width :900px}}
      [:button.button
       {:on-click #(swap! *alternatives create-new-alternative)}
       symbols/plus " Add system"]]]))

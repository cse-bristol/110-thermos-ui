(ns thermos-frontend.params.pipes
  (:require [thermos-specs.document :as document]
            [thermos-specs.path :as path]
            [thermos-frontend.inputs :as inputs]
            [reagent.core :as reagent]
            [thermos-frontend.util :refer [target-value]]
            [thermos-pages.symbols :as symbols]))

;; editor for pipe parameters
;; until we look at fixed costs for pipes, we are going to have:
;; mechanical engineering costs
;; civil engineering cost sets

;; it'd be nice to have some little graphs

(defn pipe-parameters [document]
  (reagent/with-let
    [mechanical-exponent (reagent/cursor document [::document/mechanical-cost-exponent])
     mechanical-fixed (reagent/cursor document [::document/mechanical-cost-per-m])
     mechanical-variable (reagent/cursor document [::document/mechanical-cost-per-m2])
     civil-exponent (reagent/cursor document [::document/civil-cost-exponent])
     civil-costs (reagent/cursor document [::document/civil-costs])

     max-pipe-dia (reagent/cursor document [::document/maximum-pipe-diameter])
     min-pipe-dia (reagent/cursor document [::document/minimum-pipe-diameter])
     
     flow-temperature (reagent/cursor document [::document/flow-temperature])
     return-temperature (reagent/cursor document [::document/return-temperature])
     ground-temperature (reagent/cursor document [::document/ground-temperature])

     pumping-overhead     (reagent/cursor document [::document/pumping-overhead])
     pumping-cost-per-kwh (reagent/cursor document [::document/pumping-cost-per-kwh])
     ]
    [:div
     [:div.card
      [:b "Temperatures and limits"]
      [:p "These parameters affect pipe heat losses and the relationship between diameter and power delivered."]

      [:p "Use a flow temperature of "
       [inputs/number {:value-atom flow-temperature :min 0 :max 100 :step 1}]
       "°C, "
       "a return temperature of "
       [inputs/number {:value-atom return-temperature :min 0 :max 100 :step 1}]
       "°C, and "
       "an average ground temperature of "
       [inputs/number {:value-atom ground-temperature :min 0 :max 20 :step 1}]
       "°C."
       " Allow pipes between "
       [inputs/number {:value-atom min-pipe-dia
                       :min 0 :max 2000 :step 1 :scale 1000.0}] "mm and "

       [inputs/number {:value-atom max-pipe-dia
                       :min 0 :max 2000 :step 1 :scale 1000.0}] "mm."
       
       ]
      
      ]

     [:div.card
      [:b "Pumping costs"]
      [:p
       "Pumping costs are taken to be a proportion of the system output. "
       "In a heat network they offset supply output. "
       "In a cooling network, they add to the required supply output."]
      [:p "Pumping overheads are "
       [inputs/number {:value-atom pumping-overhead :min 0 :max 100 :step 1 :scale 100}]
       " % of system output, and cost "
       [inputs/number {:value-atom pumping-cost-per-kwh
                       :min 0 :max 50 :step 0.01 :scale 100}] "c/kWh."]

      ;; TODO emissions factors for pumping
      ]
     
     [:div.card
      [:b "Mechanical engineering costs"]
      [:p "These parameters apply to every pipe, and cover the cost of buying the flow and return pipes, welding etc."]
      [:div "Calculate mechanical engineering costs as "
       [inputs/number {:value-atom mechanical-fixed :min 0 :max 5000 :step 1}]
       "¤/m + ("
       [inputs/number {:value-atom mechanical-variable :min 0 :max 5000 :step 0.1}]
       "× ⌀/m)"
       [:sup [inputs/number {:value-atom mechanical-exponent :min 0 :max 3 :step 0.01}]]
       "/m"]]

     [:div.card
      [:div {:style {:display :flex :flex-direction :row}}
       [:b "Civil engineering costs"]
       [:button.button
        {:style {:margin-left :auto}
         :on-click
         #(swap! civil-costs
                 (fn [c]
                   (let [id (inc (reduce max -1 (keys c)))]
                     (assoc c id
                            {::path/civil-cost-id id
                             ::path/civil-cost-name (str "Category " (inc id))
                             ::path/fixed-cost 0
                             ::path/variable-cost 0}))))}

        symbols/plus " Add"]
       ]
      [:p "These parameters can be different for each bit of pipe, and cover the cost of digging a hole, installing pipework, and back-filling."]
      [:p "You can set the civil engineering cost category for a pipe from the map page by selecting the path and pressing " [:b "e"] ", or by right-clicking on it."]
      (if (seq @civil-costs)
        [:div
         [:p "Calculate civil engineering costs as "
          [:em "fixed cost"]
          "¤/m + ("
          [:em "variable cost"]
          "× ⌀/m)"
          [:sup [inputs/number {:value-atom civil-exponent :min 0 :max 3 :step 0.01}]]
          "/m"]
         
         [:table {:style {:width :100%}}
          [:thead
           [:tr
            [:th "Category"]
            [:th "Fixed cost"]
            [:th "Variable cost"]
            [:th]]]
          [:tbody
           (for [id (sort (keys @civil-costs))]
             [:tr {:key id}
              [:td [inputs/text
                    :value (get-in @civil-costs [id ::path/civil-cost-name])
                    :on-change #(swap! civil-costs assoc-in [id ::path/civil-cost-name] (target-value %))
                    :placeholder "Name" :style {:width :100%}]]
              [:td [inputs/number {:style {:width :100%}
                                   :value (get-in @civil-costs [id ::path/fixed-cost])
                                   :minimum 0
                                   :maximum 5000
                                   :on-change #(swap! civil-costs assoc-in [id ::path/fixed-cost] %)
                                   }]]
              [:td [inputs/number {:style {:width :100%}
                                   :value (get-in @civil-costs [id ::path/variable-cost])
                                   :on-change #(swap! civil-costs assoc-in [id ::path/variable-cost] %)
                                   :minimum 0
                                   :step 0.1
                                   :maximum 5000
                                   }]]
              [:td [:button.button
                    {:on-click #(swap! document document/remove-civils id)}
                    symbols/dustbin]]
              ])
           ]
          ]]
        [:p "At the moment you have no civil engineering costs defined, so pipes will only have mechanical engineering costs. To define some costs, click 'Add'."]
        )
      
      ]
     ])
  )

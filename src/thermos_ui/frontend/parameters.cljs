(ns thermos-ui.frontend.parameters
  (:require [thermos-ui.frontend.virtual-table :as virtual-table]
            [thermos-ui.specs.document :as document]
            [thermos-ui.specs.technology :as technology]

            [thermos-ui.frontend.inputs :as input]
            [reagent.core :as reagent]
            [goog.object :as o]
            ))

(declare resource-editor objective-editor resources-editor)

(defn- technology-editor [{on-ok :on-ok on-cancel :on-cancel
                           initial-value :initial-value}]
  (reagent/with-let [content (reagent/atom initial-value)
                     id (reagent/cursor content [::technology/id])
                     capacity (reagent/cursor content [::technology/capacity])
                     fuel (reagent/cursor content [::technology/fuel])
                     
                     heat-eff (reagent/cursor content [::technology/heat-efficiency])
                     power-eff (reagent/cursor content [::technology/power-efficiency])
                     cost (reagent/cursor content [::technology/capital-cost])
                     ]
    [:div {:style {:position :absolute :z-index 100 :background "rgba(0,0,0,0.75)"
                   :width :100% :height :100%
                   :display :flex
                   }}
     [:div {:style {:margin :auto :background :#fff}}
      [:table
       [:tr
        [:td "Name"] [:td [input/text :value-atom id]]]
       [:tr
        [:td "Fuel"]
        [:td [input/select
              :value-atom fuel
              :values [[:gas "Gas"] [:electricity "Electricity"] [:biomass "Biomass"]]]]]
       [:tr
        [:td "Capacity"]
        [:td [input/number :value-atom capacity]]]
       [:tr
        [:td "Heat efficiency"]
        [:td
         [input/number :value-atom heat-eff :scale 100]]]
       
       [:tr
        [:td "Power efficiency"]
        [:td
         [input/number :value-atom power-eff :scale 100]]]

       [:tr
        [:td "Cost"]
        [:td [input/number :value-atom cost]]]]
      
      [:button.button {:on-click #(on-cancel)} "Cancel"]
      [:button.button {:on-click #(on-ok @content)} "OK"]]]))

(defn- years [a]
  [input/number :min 1 :step 1 :max 100 :value-atom a])


(defn- rate [a]
  [input/number :min 0 :step 0.01 :max 25 :value-atom a :scale 100])

(defn- price [a]
  [input/number :min 0 :step 0.1 :max 100 :value-atom a :scale 100]
  )

(defn- tonnes [a]
  [input/number :min 0 :max 10000 :step 1 :value-atom a])

(defn- rate-term [t r a b c]
  [:div [:span a [years t] b [rate r] c]])

(defn component [document]
  (reagent/with-let [dc (fn [& a] (reagent/cursor document a))

                     mipgap (dc ::document/mip-gap)
                     
                     system-term (dc ::document/system-term)
                     system-rate (dc ::document/system-rate)
                     plant-term (dc ::document/plant-term)
                     plant-rate (dc ::document/plant-rate)
                     network-term (dc ::document/network-term)
                     network-rate (dc ::document/network-rate)

                     gas-price     (dc ::document/in-price :gas)
                     elec-price    (dc ::document/in-price :electricity)
                     biomass-price (dc ::document/in-price :biomass)

                     elec-tariff (dc ::document/out-price :electricity)
                     heat-tariff (dc ::document/out-price :heat)

                     technologies (dc ::document/technologies)
                     
                     ef
                     (fn [fuel gas]
                       (reagent/with-let [va (dc ::document/emissions-factor gas fuel)]
                         [input/number :min 0 :max 1000 :step 1 :value-atom va
                          :scale 1000
                          ]))

                     emissions-price
                     (fn [gas]
                       (reagent/with-let [va (dc ::document/emissions-price gas)]
                         [input/number :min 0 :max 1000 :step 0.1 :value-atom va
                          :scale 1000]))
                     
                     emissions-cap
                     (fn [gas]
                       (reagent/with-let [va (dc ::document/emissions-cap gas)]
                         [input/number :min 0 :max 10000 :step 100 :value-atom va
                          :scale (/ 1 1000000)
                          ]))
                     ]
    
    [:div.parameters-component {:style {:overflow :auto}}
     [:div 
      [:h1 "Finances"]
      [:table
       [:thead
        [:tr
         [:th]
         [:th "Period (y)"]
         [:th "Rate (%)"]]]
       [:tbody
        [:tr
         [:td [:span.tooltip
               [:span.tooltiptext
                "The value being maximised is the present value "
                "of costs and revenues over this period, discounted "
                "at this rate"]
               
               "System NPV"]]
         [:td [years system-term]]
         [:td [rate system-rate]]]
        [:tr
         [:td [:span.tooltip
               [:span.tooltiptext
                "Plant capital cost will be spread over "
                "this many years with a fixed rate loan at this rate."]
               
               "Plant Capital"]]
         [:td [years plant-term]]
         [:td [rate plant-rate]]]
        [:tr
         [:td [:span.tooltip
               [:span.tooltiptext
                "Pipework capital cost will be spread over "
                "this many years with a fixed rate loan at this rate."]
               
               "Pipework Capital"]]
         [:td [years network-term]]
         [:td [rate network-rate]]]
        ]]]
     
     [:div
      [:h1 "Energy prices and factors"]

      [:table
       [:thead
        [:tr
         [:th]
         [:th "Buy (c/kWh)"]
         [:th "Sell (c/kWh)"]
         (for [[k l] document/emissions-labels]
           [:th {:key k} l " (g/kWh)"])
         ]
        ]
       [:tbody
        [:tr
         [:td "Gas"]
         [:td [price gas-price]]
         [:td]
         (for [[k l] document/emissions-labels]
           [:td {:key k} [ef :gas k]]
           )
         
         ]
        [:tr
         [:td "Biomass"]
         [:td [price elec-price]]
         [:td]
         (for [[k l] document/emissions-labels]
           [:td {:key k} [ef :biomass k]]
           )
         ]
        [:tr
         [:td [:span.tooltip
               [:span.tooltiptext
                "Emissions factors are used for consumption and avoided emissions"]
               
               "Electricity"]]
         [:td [price elec-price]]
         [:td [price elec-tariff]]
         (for [[k l] document/emissions-labels]
           [:td {:key k} [ef :electricity k]]
           )
         ]
        [:tr
         [:td [:span.tooltip
               [:span.tooltiptext
                "Emissions factors are used to calculate avoided emissions"]
               
               "Heat"]]
         [:td]
         [:td [price heat-tariff]]
         (for [[k l] document/emissions-labels]
           [:td {:key k} [ef :heat k]]
           )]]]]
     

     [:div
      [:h1 "Emissions prices and limits"]
      (for [[k l] document/emissions-labels]
        [:div {:key k}
         "Price " l " at " [emissions-price k] " ¤/t"
         " and cap emissions at " [emissions-cap k] " kt/yr"])
      ]

     [:div
      [:h1 "Optimisation"]
      [:div
       "Accept solutions within "
       [input/number :max 25 :min 0 :scale 100 :step 0.1 :value-atom mipgap]
       "% of the optimum"]
      ]
     

     [:div
      [:h1 "Plant types"]
      [:table
       [:thead
        [:tr
         [:th "Name"]
         [:th "Capacity (MW)"]
         [:th "Fuel"]
         [:th "Heat (%)"]
         [:th "Power (%)"]
         [:th "Cost (¤)"]
         [:th [:button "Add"]]]]
       [:tbody
        (for [technology (sort-by ::technology/capacity @technologies)]
          [:tr {:key (::technology/id technology)}
           [:td (::technology/id technology)]
           [:td (* 1000 (::technology/capacity technology))]
           [:td (name (::technology/fuel technology))]
           [:td (* 100 (::technology/heat-efficiency technology))]
           [:td (* 100 (::technology/power-efficiency technology))]
           [:td (.toLocaleString (::technology/capital-cost technology))]
           [:td [:button "x"] [:button "e"]]
           ]
          )
        ]
       ]
      ]
     ])
  
  )


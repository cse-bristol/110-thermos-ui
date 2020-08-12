(ns thermos-frontend.params.pipes
  (:require [thermos-specs.document :as document]
            [thermos-specs.path :as path]
            [thermos-specs.candidate :as candidate]
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

     pumping-emissions-atoms
     (into
      {}
      (for [e candidate/emissions-types]
        [e (reagent/cursor document [::document/pumping-emissions e])]))


     costs-table
     (reagent/atom
      {:diameter        [30  50  150 200 400 500 800]
       :mechanical-cost [100 200 300 400 600 800 900]
       :civil-cost   {0 {:name "Soft" :cost [100 200 300 400 600 800 900]}
                      1 {:name "Hard" :cost [150 250 400 500 600 804 900]}}
       
       }
      )
     ]
    [:div
     [:div.card
      [:b "Pipe costs"]
      [:table
       [:thead
        [:tr
         [:th "NB (mm)"]
         [:th "Capacity (kWp)"]
         [:th "Losses (kWh/m.yr)"]
         [:th "Pipe cost (¤/m)"]
         (for [[cid cc] (:civil-cost @costs-table)]
           [:th [inputs/text {:value (:name cc)}] " (¤/m)"])
         ]
        ]
       [:tbody
        (for [[i dia] (map-indexed vector (:diameter @costs-table))]
          [:tr {:key i}
           [:td [inputs/number {:value dia :min 10 :max 3000}]]
           [:td "x"]
           [:td "x"]
           [:td [inputs/number {:value (nth (:mechanical-cost @costs-table) i)
                                :min 0 :max 1000
                                }]]
           (for [[cid cc] (:civil-cost @costs-table)]
             [:td [inputs/number {:value (nth (:cost cc) i)
                                  :min 0 :max 1000
                                  }]]
             )
           ]
          )
        ]
       ]
      ]

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
                       :min 0 :max 50 :step 0.01 :scale 100}] "c/kWh. "
       "They cause emissions of"
       (interpose
        ", "
        (for [e candidate/emissions-types]
          (list
           [inputs/number {:value-atom (pumping-emissions-atoms e)
                           :min 0 :max 1000 :step 1
                           :scale (candidate/emissions-factor-scales e)
                           }]
           " "
           (candidate/emissions-factor-units e)
           " "
           (name e))))]]
     
     
     ])
  )

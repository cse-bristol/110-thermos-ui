(ns thermos-frontend.params.pipes
  (:require [thermos-specs.document :as document]
            [thermos-specs.path :as path]
            [thermos-specs.candidate :as candidate]
            [thermos-frontend.inputs :as inputs]
            [reagent.core :as reagent]
            [thermos-frontend.util :refer [target-value]]
            [thermos-pages.symbols :as symbols]
            [thermos-frontend.flow :as f]
            [thermos-util.pipes :as pipe-calcs]
            [thermos-util :refer [kw->annual-kwh]]
            [thermos-util.steam :as steam-calcs]
            [clojure.set :as set]
            [thermos-frontend.format :as format]))

(defn- match-indices [dia-index new-dias]
  (let [new-dias      (set new-dias)
        old-dias      (set (vals dia-index))
        missing       (set/difference new-dias old-dias)
        extra         (set/difference old-dias new-dias)
        spare-indices (remove (set (map dia-index new-dias))
                              (iterate dec -1))
        ]
    (loop [out      {}
           new-dias new-dias
           ix       spare-indices
           ]

      (let [[h & t]  new-dias]
        (cond
          (not h)
          out
          
          (contains? old-dias h)
          (let [i (dia-index h)]
            (recur
             (assoc out h i i h)
             t ix))

          true
          (let [i (first ix)]
            (recur
             (assoc out h i i h)
             t (rest ix))))))))

(defn- pipe-costs-table [flow]
  ;; this bit is a horrible thing to provide a stable rownumber for each row
  ;; in the face of changing diameters.

  ;; it ought to work, maybe maybe.
  (reagent/with-let [indexed-table
                     (let [dia-index (atom {})]
                       (reagent/track
                        #(let [pipe-costs @(f/view* flow ::document/pipe-costs)
                               indices
                               (swap! dia-index match-indices
                                      (keys (:rows pipe-costs)))]
                           (update
                            pipe-costs
                            :rows
                            (fn [rows]
                              (->>
                               (for [[dia a] rows]
                                 [(indices dia)
                                  (assoc a :diameter dia)])
                               (into {}))
                              )))))]
    (let [{:keys [civils rows default-civils]} @indexed-table

          pipe-parameters    @(f/view* flow pipe-calcs/select-parameters)]

      [:div.card
       [:h1.card-header "Pipe costs"]
       [:table
        [:thead
         [:tr
          [:th "NB"]
          [:th "Capacity"]
          [:th "Losses"]
          [:th "Pipe cost"]
          (when-not (empty? civils)
            [:th {:col-span (count civils)} "Civil cost (¤/m)"])
          [:th]
          ]
         [:tr
          [:th "mm"]
          [:th "Wp"]
          [:th "kWh/m.yr"]
          [:th "¤/m"]
          (for [[cid cc] civils]
            [:th {:key cid}
             [:div {:style {:display :flex
                            :max-width :6em}}
              [inputs/text {:style
                            {:flex-grow 1
                             :font-weight :normal
                             :flex-shrink 1
                             :width 1
                             }

                            :class "square-right"
                            :value cc
                            :placeholder "Cost name"
                            :on-change
                            (fn [el]
                              (f/fire!
                               flow
                               [:pipe/rename-civils
                                cid (-> el .-target .-value)]))
                            }]
              [:button.button.button--outline.square-left
               {:style {:padding 0 :border-left :none
                        :height :30px
                        }
                :title "Remove column"
                :on-click
                #(f/fire!
                  flow
                  [:pipe/remove-civils cid])}
               symbols/delete]]]
            )
          [:th]
          ]
         ]
        [:tbody
         (for [[ix costs] (sort-by (comp :diameter second) rows)
               :let [dia (:diameter costs)]]
           [:tr {:key ix}
            [:td [inputs/number {:value dia :min 10 :max 3000
                                 :style {:max-width :5em}
                                 :on-blur
                                 (fn [v]
                                   (f/fire!
                                    flow [:pipe/change-diameter dia v]))
                                 }]]
            [:td
             [inputs/parsed
              {:class "input number-input"
               :parse
               (fn [x]
                 (if (= "" x) :empty
                     (format/parse-si-number x)))
               
               :render
               (fn [x]
                 (if (= :empty x)
                   nil
                   (format/si-number x)))
               
               :value (:capacity-kwp costs)
               :style {:max-width :5em}
               :on-change
               (fn [v]
                 (f/fire!
                  flow [:pipe/change-capacity dia
                        (if (= :empty v) nil v)]))
               
               :placeholder
               (format/si-number
                (* 1000.0 (pipe-calcs/power-for-diameter pipe-parameters dia)))}]]
            
            [:td (let [default-loss (pipe-calcs/losses-for-diameter pipe-parameters dia)
                       given-loss (:losses-kwh costs)]
                   [inputs/number {:style {:max-width :5em
                                           :border (when-not (or given-loss default-loss)
                                                     "2px red solid")
                                           }
                                   :value
                                   given-loss

                                   :empty-value [nil nil]
                                   
                                   :placeholder default-loss
                                   :pattern  (when-not default-loss ".+")
                                   :required (when-not default-loss true)
                                   
                                   :max 10000
                                   :min 1

                                   :on-change
                                   (fn [v]
                                     (f/fire!
                                      flow [:pipe/change-losses dia v]))
                                   
                                   }])
             ]
            ;; mech cost
            [:td [inputs/number {:style {:max-width :6em}
                                 :value (:pipe costs)
                                 :min 0 :max 1000
                                 :on-change
                                 (fn [v]
                                   (f/fire!
                                    flow [:pipe/change-cost dia v]))
                                 }]]
            (for [[cid cc] civils]
              [:td {:key cid}
               [inputs/number {:style {:max-width :6em}
                               :value (get costs cid)
                               :min 0 :max 1000
                               :on-change
                               (fn [v]
                                 (f/fire!
                                  flow [:pipe/change-civil-cost dia cid v]))
                               }]]
              )
            
            [:td
             [:button.button.button--circular.button--outline
              {:on-click #(f/fire! flow [:pipe/remove-diameter dia])
               :title "Remove row"}
              symbols/delete]]
            ]
           )
        
         ]
        ]
       [:div {:style {:margin-top :1em}}
        [:button.button
         {:on-click
          #(let [max-dia (reduce max 0 (map :diameter (vals rows)))]
             (f/fire!
              flow
              [:pipe/add-diameter (+ max-dia 50)])
             
             )
          }
         "Add diameter"]
        [:button.button
         {:on-click
          #(f/fire! flow [:pipe/add-civils (str "Cost "
                                                (inc (count civils)))])
          
          }
         "Add civil costs"]

        [:label {:style {:margin-left :1em}}
         "Default civil costs: "
         [inputs/select
          {:value default-civils
           :values (conj (vec civils) [nil "None"])
           :on-change (fn [cid]
                        (f/fire! flow [:pipe/set-default-civils cid]))}]]
        ]
       ]))
  )

(defn- bar-g->MPa [bar-g]
  ;; 1 bar = 100kpa, so 0.1bar = 1MPa
  ;; 1 bar-g = 2 bar (in atmosphere)
  (* (+ bar-g 1.0) 0.1))

(defn- MPa->bar-g [MPa]
  (- (* 10 MPa) 1.0))

(defn cost-model [flow]
  (let [medium         @(f/view* flow ::document/medium :hot-water)]
    [:div.card
     [:h1.card-header "Capacity & loss model"]
     [:div.flex-cols
      [:div
       [:div
        [:label {:style {:font-size :1.5em}}
         [:input {:type      :radio :checked (= :hot-water medium)
                  :on-change #(f/fire! flow [:pipe/change-medium :hot-water])
                  :value     "cost-model"
                  }]
         "Hot water"]
        
        ]
       
       
       [:div
        [:label {:style {:font-size :1.5em}}
         [:input {:type      :radio
                  :value     "cost-model"
                  :checked   (= :saturated-steam medium)
                  :on-change #(f/fire! flow [:pipe/change-medium :saturated-steam])
                  }]
         "Saturated steam"]

        (case medium
          :hot-water
          [:div {:key :hot-water-controls
                 :style {:margin-left           :2em
                         :margin-top            :1em
                         :display               :grid
                         :grid-template-columns "10em 5em auto"
                         :gap                   :0.2em
                         }}
           [:label "Flow temperature: "]
           [inputs/number
            {:value @(f/view* flow ::document/flow-temperature)
             :on-change #(f/fire! flow [:pipe/change-flow-temperature %])}
            ]
           [:label "℃"]
           
           [:label "Return temperature:"]
           [inputs/number
            {:value     @(f/view* flow ::document/return-temperature)
             :on-change #(f/fire! flow [:pipe/change-return-temperature %])
             }]
           [:label "℃"]

           [:label "Ground temperature:"]
           [inputs/number
            {:value @(f/view* flow ::document/ground-temperature)
             :on-change #(f/fire! flow [:pipe/change-ground-temperature %])}]
           [:label "℃"]
           ]

          :saturated-steam
          [:div {:key :steam-controls
                 :style {:margin-left           :2em
                         :margin-top            :1em
                         :display               :grid
                         :grid-template-columns "10em 5em auto"
                         :gap                   :0.2em
                         }}
           [:label "Steam pressure: "]
           [inputs/number {:value (MPa->bar-g @(f/view* flow ::document/steam-pressure))
                           :on-change #(f/fire! flow [:pipe/change-steam-pressure (bar-g->MPa %)])}]
           [:label
            {:style {:white-space :nowrap}}
            "bar g"]
           
           [:label "Velocity: "]
           [inputs/number {:value @(f/view* flow ::document/steam-velocity)
                           :on-change #(f/fire! flow [:pipe/change-steam-velocity %])
                           }
            ]
           [:label "m/s"]

           [:label "Ground temperature:"]
           [inputs/number {:value @(f/view* flow ::document/ground-temperature)
                           :on-change #(f/fire! flow [:pipe/change-ground-temperature %])}
            ]
           [:label "℃"]

           ]

          [:div "Unknown medium!"]
          )
        
        ]]

      [:div {:style {:margin-left :2em :font-size :1.1em}}
       (case medium
         :hot-water
         [:<>
          [:p
           "Pipe capacity is calculated from diameter using "
           [:a {:target "help"
                :href "/help/calculations.html#pipe-diameter-calc"}
            "recommended flow rates for the diameter"]
           ", the specific heat of water, and the flow/return difference."
           ]
          [:p
           "Heat losses are calculated from diameter using "
           [:a {:target "help"
                :href "/help/calculations.html#pipe-heat-losses"} "this model"] "."
           ]]
         :saturated-steam
         [:<>
          [:p
           "Pipe capacity is calculated from diameter and velocity using the "
           "specific enthalpy of vaporisation & density for saturated steam "
           "at the given pressure."]
          [:p
           "Heat losses are calculated by interpolating from a table for unlagged pipes, "
           "and then applying an insulation factor for 50mm insulation. "
           "These tables only cover diameters from 15-150mm diameter; you will have to manually "
           "enter losses for diameters outside this range, or they will be ignored by the model."
           ]
          [:p
           "Note that the default pipe costs are for hot water pipes, not steam pipes."
           ]
          ]

         [:p "Unexpected medium: " medium]
         
         )
       ]
      ]
     ]))

(defn pipe-parameters [document flow]
  (reagent/with-let
    [flow-temperature (reagent/cursor document [::document/flow-temperature])
     return-temperature (reagent/cursor document [::document/return-temperature])
     ground-temperature (reagent/cursor document [::document/ground-temperature])

     pumping-overhead     (reagent/cursor document [::document/pumping-overhead])
     pumping-cost-per-kwh (reagent/cursor document [::document/pumping-cost-per-kwh])

     pumping-emissions-atoms
     (into
      {}
      (for [e candidate/emissions-types]
        [e (reagent/cursor document [::document/pumping-emissions e])]))
     ]
    [:div
     [pipe-costs-table flow]
     [cost-model flow]
     ;; [:div.card
     ;;  [:b "Temperatures and limits"]
     ;;  [:p "These parameters affect pipe heat losses and the relationship between diameter and power delivered."]

     ;;  [:p "Use a flow temperature of "
     ;;   [inputs/number {:value-atom flow-temperature :min 0 :max 100 :step 1}]
     ;;   "°C, "
     ;;   "a return temperature of "
     ;;   [inputs/number {:value-atom return-temperature :min 0 :max 100 :step 1}]
     ;;   "°C, and "
     ;;   "an average ground temperature of "
     ;;   [inputs/number {:value-atom ground-temperature :min 0 :max 20 :step 1}]
     ;;   "°C."
     ;;   " Allow pipes between "
     ;;   [inputs/number {:value-atom min-pipe-dia
     ;;                   :min 0 :max 2000 :step 1 :scale 1000.0}] "mm and "

     ;;   [inputs/number {:value-atom max-pipe-dia
     ;;                   :min 0 :max 2000 :step 1 :scale 1000.0}] "mm."
       
     ;;   ]
      
     ;;  ]


     [:div.card
      [:h1.card-header "Pumping costs"]
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
          [:<> {:key e}
           [inputs/number {:value-atom (pumping-emissions-atoms e)
                           :min 0 :max 1000 :step 1
                           :scale (candidate/emissions-factor-scales e)
                           }]
           " "
           (candidate/emissions-factor-units e)
           " "
           (name e)]))]]
     
     
     ])
  )

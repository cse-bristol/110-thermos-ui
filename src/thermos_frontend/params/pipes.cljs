;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.params.pipes
  (:require [thermos-specs.document :as document]
            [thermos-specs.path :as path]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.tariff :as tariff]
            [thermos-frontend.inputs :as inputs]
            [reagent.core :as reagent]
            [thermos-frontend.util :refer [target-value]]
            [thermos-pages.symbols :as symbols]
            [re-frame.core :as rf]
            [thermos-util.pipes :as pipe-calcs]
            [thermos-util :refer [kw->annual-kwh]]
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

(defn- pipe-costs-table []
  ;; this bit is a horrible thing to provide a stable rownumber for each row
  ;; in the face of changing diameters.

  ;; it ought to work, maybe maybe.
  (reagent/with-let [indexed-table
                     (let [dia-index (atom {})]
                       (reagent/track
                        #(let [pipe-costs @(rf/subscribe [:doc/pipe-costs])
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

          pipe-parameters    @(rf/subscribe [:doc/pipe-parameters])]

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
                              (rf/dispatch [:pipe/rename-civils
                                cid (-> el .-target .-value)]))
                            }]
              [:button.button.button--outline.square-left
               {:style {:padding 0 :border-left :none
                        :height :30px
                        }
                :title "Remove column"
                :on-click
                #(rf/dispatch [:pipe/remove-civils cid])}
               symbols/delete]]]
            )
          [:th]
          ]
         ]
        [:tbody
         (for [[ix costs] (sort-by (comp :diameter second) rows)
               :let [dia (:diameter costs)]]
           [:tr {:key ix}
            [:td [inputs/number2 {:value dia :min 10 :max 3000
                                  :style {:max-width :5em}
                                  :on-blur
                                  (fn [v]
                                    (rf/dispatch [:pipe/change-diameter dia v]))
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
               
               :value (some-> costs :capacity-kw (* 1000))
               :style {:max-width :5em}
               :on-change
               (fn [_ v]
                 (rf/dispatch [:pipe/change-capacity dia
                        (if (= :empty v) nil (/ v 1000.0))]))
               
               :placeholder
               (format/si-number
                (* 1000.0 (pipe-calcs/power-for-diameter pipe-parameters dia)))}]]
            
            [:td (let [default-loss (pipe-calcs/losses-for-diameter pipe-parameters dia)
                       given-loss (:losses-kwh costs)]
                   [inputs/number2 {:style {:max-width :5em
                                            :border (when-not (or given-loss default-loss)
                                                      "2px red solid")
                                            }
                                    :value
                                    given-loss

                                    :empty-value [nil (and default-loss
                                                           (.toFixed default-loss 2))]

                                    :pattern  (when-not default-loss ".+")
                                    :required (when-not default-loss true)
                                    
                                    :max 10000
                                    :min 1

                                    :on-change
                                    (fn [v]
                                      (rf/dispatch [:pipe/change-losses dia v]))
                                    
                                    }])
             ]
            ;; mech cost
            [:td [inputs/number2 {:style {:max-width :6em}
                                  :value (:pipe costs)
                                  :min 0 :max 1000
                                  :on-change
                                  (fn [v]
                                    (rf/dispatch [:pipe/change-cost dia v]))
                                  }]]
            (for [[cid cc] civils]
              [:td {:key cid}
               [inputs/number2 {:style {:max-width :6em}
                                :value (get costs cid)
                                :min 0 :max 1000
                                :on-change
                                (fn [v]
                                  (rf/dispatch [:pipe/change-civil-cost dia cid v]))
                                }]]
              )
            
            [:td
             [:button.button.button--circular.button--outline
              {:on-click #(rf/dispatch [:pipe/remove-diameter dia])
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
             (rf/dispatch [:pipe/add-diameter (+ max-dia 50)])
             
             )
          }
         "Add diameter"]
        [:button.button
         {:on-click
          #(rf/dispatch [:pipe/add-civils (str "Cost "
                                                (inc (count civils)))])
          
          }
         "Add civil costs"]

        [:label {:style {:margin-left :1em}}
         "Default civil costs: "
         [inputs/select
          {:value default-civils
           :values (conj (vec civils) [nil "None"])
           :on-change (fn [cid]
                        (rf/dispatch [:pipe/set-default-civils cid]))}]]
        ]
       ]))
  )

(defn cost-model []
  [:div.card {:key :a}
   [:h1.card-header "Capacity & loss model"]
   [:div.flex-cols {:key :b}
    [:div {:key :hot-water-controls
           :style {:display               :grid
                   :grid-template-columns "10em 5em auto"
                   :gap                   :0.2em
                   }}
     [:label "Flow temperature: "]
     [inputs/number2
      {:key :flow-temperature
       :value @(rf/subscribe [:doc/get [::document/flow-temperature]])
       :on-change #(rf/dispatch [:pipe/change-flow-temperature %])}
      ]
     [:label "℃"]

     [:label "Return temperature:"]
     [inputs/number2
      {:value     @(rf/subscribe [:doc/get [::document/return-temperature]])
       :on-change #(rf/dispatch [:pipe/change-return-temperature %])
       }]
     [:label "℃"]

     [:label "Ground temperature:"]
     [inputs/number2
      {:value @(rf/subscribe [:doc/get [::document/ground-temperature]])
       :on-change #(rf/dispatch [:pipe/change-ground-temperature %])}]
     [:label "℃"]
     ]

    [:div {:key :docs :style {:margin-left :2em :font-size :1.1em}}
     [:p
      "Pipe capacity is calculated from diameter using "
      [:a {:target "help"
           :href "/help/network/technical-description.html#pipe-diameter-calc"}
       "recommended flow rates for the diameter"]
      ", the specific heat of water, and the flow/return difference."
      ]
     [:p
      "Heat losses are calculated from diameter using "
      [:a {:target "help"
           :href "/help/network/technical-description.html#pipe-heat-losses"} "this model"] "."
      ]]
    ]
   ])

(def conn-fixed-unit "¤")
(def conn-var-unit "¤/kWp")

(defn- connection-cost-row
  [{id :key} *document *connection-costs]
  (let [get #(get-in @*connection-costs [id %])
        put #(swap! *connection-costs assoc-in [id %1] %2)
        delete-connection-cost #(swap! *document document/remove-connection-cost id)]
    [:tr {:key id}
     [:td [inputs/text
           {:placeholder (str "Connection cost " id)
            :value (get ::tariff/name)
            :on-change #(put ::tariff/name (target-value %))}]]
     [:td [:label
           [inputs/number2
            {:title "The fixed part of the capital cost of connecting a building."
             :style {:max-width :5em}
             :max 1000
             :min 0
             :value (get ::tariff/fixed-connection-cost)
             :on-change #(put ::tariff/fixed-connection-cost %)
             }]
           " " conn-fixed-unit]]
     [:td [:label
           [inputs/number2
            {:title "The variable part of the capital cost of connecting a building."
             :style {:max-width :5em}
             :max 100
             :min 0
             :step 0.1
             :value (get ::tariff/variable-connection-cost)
             :on-change #(put ::tariff/variable-connection-cost %)
             }]
           " " conn-var-unit]]

     [:td {:style {:width :1px}}
      [:button.button {:on-click delete-connection-cost}
       symbols/dustbin]]
     ]))

(defn- connection-costs [doc]
  (reagent/with-let [*connection-costs (reagent/cursor doc [::document/connection-costs])]
    [:div.card
    [:h2.card-header "Connection Costs"]
    [:p "Each building also has associated connection costs, which determine the capital costs of connecting the building to the network. These costs are borne by the network operator."]

    (when (seq @*connection-costs)
      [:table.table {:style {:max-width :700px}}
       [:thead
        [:tr
         [:th "Connection cost name"]
         [:th "Fixed cost"]
         [:th "Capacity cost"]
         [:th]]]
       [:tbody
        (doall
         (for [id (sort (keys @*connection-costs))]
           [connection-cost-row {:key id} doc *connection-costs]))]])

    [:div.centre {:style {:max-width :700px}}
     [:button.button
      {:style {:margin-top :1em}
       :on-click #(swap! *connection-costs
                         (fn [t]
                           (let [id (inc (reduce max -1 (keys t)))]
                             (assoc
                              t
                              id
                              {::tariff/name ""
                               ::tariff/cc-id id
                               ::tariff/fixed-connection-cost 0
                               ::tariff/variable-connection-cost 0
                               }))))}
      symbols/plus " Add connection cost"]]

    ]))

(defn pipe-parameters [document]
  (reagent/with-let
    [flow-temperature (reagent/cursor document [::document/flow-temperature])
     return-temperature (reagent/cursor document [::document/return-temperature])
     ground-temperature (reagent/cursor document [::document/ground-temperature])

     pumping-overhead     (reagent/cursor document [::document/pumping-overhead])
     pumping-cost-per-kwh (reagent/cursor document [::document/pumping-cost-per-kwh])

     diversity-limit      (reagent/cursor document [::document/diversity-limit])
     diversity-rate       (reagent/cursor document [::document/diversity-rate])
     
     pumping-emissions-atoms
     (into
      {}
      (for [e candidate/emissions-types]
        [e (reagent/cursor document [::document/pumping-emissions e])]))
     ]
    [:div {:key :pipe-parameters}
     ^{:key :cost-model} [cost-model]
     [pipe-costs-table]
     [connection-costs document]

     [:div.card
      [:h1.card-header "Diversity function"]
      [:p "Calculate diversity as "
       [inputs/number2 {:value-atom diversity-limit :min 0 :max 100 :step 1 :scale 100
                        :style {:max-width :5em}}]
       
       "% + " (str (* 100 (- 1 @diversity-limit)))
       "% / ("
       [inputs/number2 {:value-atom diversity-rate :min 1 :max 100 :step 1 :scale 1
                        :style {:max-width :5em}}]
       "✕ N"
       ")"]]
          
     [:div.card
      [:h1.card-header "Pumping costs"]
      [:p
       "Pumping costs are taken to be a proportion of the system output. "
       "In a heat network they offset supply output. "
       "In a cooling network, they add to the required supply output."]
      [:p "Pumping overheads are "
       [inputs/number2 {:value-atom pumping-overhead :min 0 :max 100 :step 1 :scale 100
                        :style {:max-width :5em}}]
       " % of system output, and cost "
       [inputs/number2 {:value-atom pumping-cost-per-kwh
                        :style {:max-width :5em}
                       :min 0 :max 50 :step 0.01 :scale 100}] "c/kWh. "
       "They cause emissions of"
       (interpose
        ", "
        (for [e candidate/emissions-types]
          [:<> {:key e}
           [inputs/number2 {:value-atom (pumping-emissions-atoms e)
                            :style {:max-width :5em}
                            :min 0 :max 1000 :step 1
                            :scale (candidate/emissions-factor-scales e)
                            }]
           " "
           (candidate/emissions-factor-units e)
           " "
           (name e)]))]]
     
     
     ])

  )

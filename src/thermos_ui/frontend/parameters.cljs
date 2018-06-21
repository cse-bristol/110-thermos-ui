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


(defn component [document]
  ;; we need to get some flexbox in this
  (reagent/with-let [tech (reagent/cursor document [::document/technologies])
                     editing-technology (reagent/atom nil)
                     ]
    [:div {:style {:height "calc(100% - 100px)"
                   :display :flex
                   :flex-direction :column
                   }}
     [:div {:style {:display :flex :flex-direction :row}}
      [objective-editor document]
      ]

     [:div {:style {:flex 1 :display :flex :flex-direction :column}}
      
      [:div {:style {:flex-grow 1}}
       (when @editing-technology
         [technology-editor {:on-ok
                             (fn [new-value]
                               ;; TODO replace the element
                               (swap! tech (fn [techs]
                                             (let [old-value @editing-technology]
                                               (if (some (partial = old-value) techs)
                                                 (replace {old-value new-value} techs)
                                                 (conj techs new-value))
                                               )))
                               (reset! editing-technology nil))
                             
                             :on-cancel #(reset! editing-technology nil)
                             :initial-value @editing-technology
                             }])
       
       [:div {:style {:display :flex :margin :0.5em}}
        [:h1 {:style {:font-weight :bold
                     :font-size :18pt
                     :display :inline-block}}
        "Technologies"]
        [:button.button {:style {:margin-left :auto}
                         :on-click #(reset! editing-technology {::technology/fuel :gas})}
         "Add ⊕"]]
       
       [virtual-table/component
        {:items @tech
         :rowHeight 30
         :headerHeight 30}

        ;; we want all these to be editable or we want an edit form
        ;; that we can pop up over the top (might be easier?)
        {:key ::technology/id :label "Name"}
        {:key ::technology/fuel :label "Fuel"
         :cellRenderer virtual-table/render-keyword}
        
        {:key ::technology/capacity :label "MW"}
        
        {:key ::technology/heat-efficiency :label "Heat %"
         :cellRenderer (virtual-table/render-number :scale 100 :unit "%")}
        {:key ::technology/power-efficiency :label "Power %"
         :cellRenderer (virtual-table/render-number :scale 100 :unit "%")
         }
        
        {:key ::technology/capital-cost :label "Cost"
         :cellRenderer (virtual-table/render-number)
         }
        {:key :edit :label "" :disableSort true
         :cellRenderer
         (fn [a] (reagent/as-element [:button.button
                                      {:on-click
                                       #(reset! editing-technology (o/get a "rowData" nil))
                                       }
                                      "Edit"]))
         }
        {:key :edit :label "" :disableSort true
         :cellRenderer
         (fn [a] (let [item (o/get a "rowData")]
                   (reagent/as-element [:button.button
                                        {:on-click #(swap! tech
                                                           (fn [x] (remove #{item} x)))}
                                        "Remove"]
                                       )))
         }
        ]]]]))

(defn- objective-editor [document]
  (reagent/with-let
    [gap (reagent/cursor document [::document/objective ::document/gap])

     plant-period (reagent/cursor document [::document/objective ::document/plant-period])
     plant-interest-rate (reagent/cursor document [::document/objective ::document/plant-interest-rate])

     network-period (reagent/cursor document [::document/objective ::document/network-period])
     network-interest-rate (reagent/cursor document [::document/objective ::document/network-interest-rate])

     carbon-cost (reagent/cursor document [::document/objective ::document/carbon-cost])

     carbon-cap (reagent/cursor document [::document/objective ::document/carbon-cap])

     electricity-import-price (reagent/cursor document [::document/objective ::document/electricity-import-price])
     electricity-export-price (reagent/cursor document [::document/objective ::document/electricity-export-price])

     heat-price (reagent/cursor document [::document/objective ::document/heat-price])
     
     gas-price (reagent/cursor document [::document/objective ::document/gas-price])
     biomass-price (reagent/cursor document [::document/objective ::document/biomass-price])

     electricity-carbon (reagent/cursor document [::document/objective ::document/electricity-emissions])
     gas-carbon (reagent/cursor document [::document/objective ::document/gas-emissions])
     biomass-carbon (reagent/cursor document [::document/objective ::document/biomass-emissions])
     ]
    [:div {:style {:flex 1 :display :flex :flex-direction :row :margin-top :1em}}
     [:div {:style {:margin-left :auto :margin-right :auto}}
      [:h1 {:style {:font-weight :bold :font-size :18pt :display :block}}
       "Capital costs"]

      [:table
       [:thead
        [:tr [:th ""] [:th "Period"] [:th "Rate"]]]
       
       [:tbody
        
        [:tr [:td "Plant"]
         [:td [input/number :value-atom plant-period :step 1 :max 50 :min 1]]
         [:td [input/number :value-atom plant-interest-rate :step 0.01 :max 50 :min 1]]]

        [:tr [:td "Network"]
         [:td [input/number :value-atom network-period :step 1 :max 50 :min 1]]
         [:td [input/number :value-atom network-interest-rate :step 0.01 :max 50 :min 1]]]
        ]
       ]
      ]
     
     [:div {:style {:margin-left :auto :margin-right :auto}}
      [:h1 {:style {:font-weight :bold :font-size :18pt :display :block}}
       "Resource costs"]
      [:table
       [:thead
        [:tr [:td "Resource"] [:td "Cost"] [:td "Revenue"] [:td "Carbon"]]]
       [:tbody
        (for [[name {cost :cost revenue :revenue carbon :carbon}]
              {"Electricity" {:cost electricity-import-price :revenue electricity-export-price
                              :carbon electricity-carbon}
               "Gas" {:cost gas-price :carbon gas-carbon}
               "Biomass" {:cost biomass-price :carbon biomass-carbon}
               "Heat" {:revenue heat-price}}
              ]
          [:tr {:key name}
           [:td name]
           [:td (when cost [input/number :value-atom cost :step 0.01 :max 9 :min 0])]
           [:td (when revenue [input/number :value-atom revenue :step 0.01 :max 9 :min 0])]
           [:td (when carbon [input/number :value-atom carbon :step 0.01 :max 9 :min 0])]])]
       ]]
     [:div {:style {:margin-left :auto :margin-right :auto}}
      [:h1 {:style {:font-weight :bold :font-size :18pt :display :block}}
       "Emissions limits"]

      [:table
       [:tbody
        [:tr
         [:td "Cost of carbon"]
         [:td [input/number :value-atom carbon-cost :step 0.01 :min 0 :max 100]]
         ]
        [:tr
         [:td "Carbon limit"]
         [:td [input/number :value-atom carbon-cap :step 1 :max 1e12 :min 0]]
         ]
        ]
       ]

      [:h1 {:style {:margin-top :1em :font-weight :bold :font-size :18pt :display :block}}
       "Acceptable quality"]
      [:table [:tbody [:tr
                       [:td [input/number :value-atom gap :max 10 :min 0 :step 0.1 :scale 100]]
                       [:td "% from best"]]]]]
     
     ]))

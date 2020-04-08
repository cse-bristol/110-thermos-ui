(ns thermos-frontend.params.supply-technologies
  "Editor for supply problem technology definitions"
  (:require [reagent.core :as reagent]
            [thermos-frontend.inputs :as inputs]
            [thermos-frontend.debug-box :as debug]
            ))


(def empty-cost {:fixed 0 :per-kwh 0 :per-kwp 0})

(defn new-plant [name]
  {:capital-cost empty-cost
   :operating-cost empty-cost
   :lifetime 25
   :fuel "Electricity"
   :chp false
   :capacity-kwp 1000
   :power-efficiency nil
   :heat-efficiency 1.0
   :substation nil
   :name name})

(defn new-substation [name] {:name name :headroom-kwp 10000 :alpha 0.8})

(defn new-storage [name]    {:name name :lifetime 50 :capacity-kwh 10000 :efficiency 0.9 :capital-cost empty-cost})

(defn cost-cells [cost-atom]
  (list
   [:td {:key :fixed}
    [inputs/number
     {:value-atom (reagent/cursor cost-atom [:fixed])
      :min 0 :max 1000 :scale (/ 1 1000)}]]
   [:td {:key :capacity}
    [inputs/number
     {:value-atom (reagent/cursor cost-atom [:per-kwp])
      :min 0 :max 1000}
     ]]
   [:td {:key :output}
    [inputs/number
     {:value-atom (reagent/cursor cost-atom [:per-kwh])
      :min 0 :max 1000
      }]]))

(defn supply-tech-parameters [doc]
  (reagent/with-let [plant-options (reagent/cursor doc [:supply/plant-options])
                     storage-options (reagent/cursor doc [:supply/storage-options])
                     substations (reagent/cursor doc [:supply/substations])
                     fuel-options (reagent/track
                                   #(let [fuels (:fuel (:supply/profiles @doc))]
                                      (for [k (sort (keys fuels))]
                                        [k k])))
                     ]
    [:div.card.flex-grow.parameters-component
     [:h1 "Supply technologies"]
     [:div {:style {:overflow-x :auto}}
      [:table 
       [:thead
        [:tr
         [:th {:col-span 8}]
         [:th {:col-span 3} "Capital cost"]
         [:th {:col-span 3} "Operating cost"]
         ]
        [:tr
         [:th "Technology"]
         [:th "Lifetime"]
         [:th "Fuel"]
         [:th "CHP"]
         [:th "Capacity"]
         [:th "Power/fuel"]
         [:th "Heat/fuel"]
         [:th "Substation"]
         [:th "k¤"]
         [:th "¤/kWp"]
         [:th "¤/kWh"]
         [:th "k¤"]
         [:th "¤/kWp"]
         [:th "¤/kWh"]
         ]]
       [:tbody
        (doall
         (for [[plant-option params] (sort-by first @plant-options)]
           [:tr {:key plant-option}
            [:td [inputs/text {:value-atom (reagent/cursor plant-options [plant-option :name])
                               :placeholder "Plant type"
                               :style {:width :8em}
                               }
                  ]
             ]
            [:td [inputs/number {:value-atom (reagent/cursor plant-options [plant-option :lifetime])
                                 :style {:width :3em}
                                 :min 1 :max 100}]]
            [:td [inputs/select
                  {:value-atom (reagent/cursor plant-options [plant-option :fuel])
                   :values @fuel-options}]]
            [:td [inputs/check
                  {:on-change #(swap! plant-options assoc-in [plant-option :chp] %)
                   :value (-> params :chp)}]]
            [:td [inputs/number
                  {:value-atom (reagent/cursor plant-options [plant-option :capacity-kwp])
                   :scale (/ 1 1000.0)
                   :step 1
                   :min 1 :max 1000}]]
            [:td (if (:chp params)
                   [inputs/number
                    {:value-atom (reagent/cursor plant-options [plant-option :power-efficiency])
                     :scale 100
                     :step 0.1
                     :min 1 :max 100}]
                   [:em "n/a"]
                   )]
            [:td [inputs/number
                  {:value-atom (reagent/cursor plant-options [plant-option :heat-efficiency])
                   :scale 100
                   :step 0.1
                   :min 1 :max 100}]]
            [:td
             [inputs/select
              {:value-atom (reagent/cursor plant-options [plant-option :substation])
               :values (cons
                        [nil "None"]
                        (for [[id {name :name}] (sort-by first @substations)]
                          [id name]))}]]
            
            (cost-cells
             (reagent/cursor plant-options [plant-option :capital-cost]))
            
            (cost-cells
             (reagent/cursor plant-options [plant-option :operating-cost]))
            ]))
        [:tr [:td {:col-span 10}
              [:input {:type :text :placeholder "New plant option"
                       :style {:width :100%}
                       :on-key-up
                       #(when (= (.-key %) "Enter")
                          (let [pname (.. % -target -value)]
                            (swap! plant-options assoc pname ;; ID int?
                                   (new-plant pname))))}]]]]
       ]]

     [:h1 "Substations"]
     [:table
      [:thead
       [:tr [:th "Name"] [:th "Headroom"] [:th "Alpha"]]]
      
      [:tbody
       (for [[id params] (sort-by first @substations)]
         [:tr {:key id}
          [:td [inputs/text {:value-atom (reagent/cursor substations [id :name]) :placeholder "Name"}]]
          [:td [inputs/number
                {:value-atom (reagent/cursor substations [id :headroom-kwp])
                 :scale (/ 1 1000.0)
                 :step 1
                 :min 1 :max 1000}]]
          [:td [inputs/number
                {:value-atom (reagent/cursor substations [id :alpha])
                 :scale 100
                 :step 0.1
                 :min 1 :max 100}]]
          ]
         )
       [:tr [:td {:col-span 3}
             [:input {:type :text :placeholder "New substation"
                      :style {:width :100%}
                      :on-key-up
                      #(when (= (.-key %) "Enter")
                         (let [pname (.. % -target -value)]
                           (swap! substations assoc pname ;; ID int?
                                  (new-substation pname))))}]]]
       ]]
     
     [:h1 "Storage technologies"]
     [:table
      [:thead
       [:tr [:th {:col-span 4}] [:th {:col-span 3} "Capital cost"]]
       [:tr [:th "Name"] [:th "Lifetime"] [:th "Capacity"] [:th "Cycle efficiency"]
        [:th "k¤"] [:th "¤/kWp"] [:th "¤/kWh"]]]
      
      [:tbody
       (for [[id params] (sort-by first @storage-options)]
         [:tr {:key id}
          [:td [inputs/text {:value-atom (reagent/cursor storage-options [id :name]) :placeholder "Name"}]]
          [:td [inputs/number {:value-atom (reagent/cursor storage-options [id :lifetime])
                               :min 1 :max 100}]]
          [:td [inputs/number
                {:value-atom (reagent/cursor storage-options [id :capacity-kwh])
                 :scale (/ 1 1000.0)
                 :step 1
                 :min 1 :max 1000}]]
          [:td [inputs/number
                {:value-atom (reagent/cursor storage-options [id :efficiency])
                 :scale 100
                 :step 0.1
                 :min 1 :max 100}]]
          (cost-cells
           (reagent/cursor storage-options [id :capital-cost]))
          ]

         )

       [:tr [:td {:col-span 3}
             [:input {:type :text :placeholder "New storage technology"
                      :style {:width :100%}
                      :on-key-up
                      #(when (= (.-key %) "Enter")
                         (let [pname (.. % -target -value)]
                           (swap! storage-options assoc pname ;; ID int?
                                  (new-storage pname))))}]]]
       ]]
     
     
     ])
  )


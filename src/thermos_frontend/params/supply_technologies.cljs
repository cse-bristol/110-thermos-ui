(ns thermos-frontend.params.supply-technologies
  "Editor for supply problem technology definitions"
  (:require [reagent.core :as reagent]
            [thermos-frontend.inputs :as inputs]
            [thermos-frontend.debug-box :as debug]
            [thermos-specs.supply :as supply]
            [clojure.spec.alpha :as s]
            [thermos-pages.symbols :as syms]
            [clojure.set :as set]))

(def empty-cost {:fixed 0 :per-kwh 0 :per-kwp 0})

(defn new-plant [name]
  {:capital-cost empty-cost
   :operating-cost empty-cost
   :lifetime 25
   :fuel 0
   :chp false
   :capacity-kwp 1000
   :power-efficiency nil
   :heat-efficiency 1.0
   :substation nil
   :name name})

(defn new-substation [name] {:name name :headroom-kwp 10000 :alpha 0.8})

(defn new-storage [name]    {:name name :lifetime 50
                             :capacity-kwh 10000
                             :capacity-kwp 50
                             :efficiency 0.9 :capital-cost empty-cost})

(defn- delete-button [atts]
  [:button.button--link-style atts syms/delete])

(defn cost-cells [cost-atom]
  (list
   [:td {:key :fixed}
    [inputs/number
     {:value-atom (reagent/cursor cost-atom [:fixed])
      :style {:width :4em}
      :min 0 :max 1000 :scale (/ 1000.0)}]]
   [:td {:key :capacity}
    [inputs/number
     {:value-atom (reagent/cursor cost-atom [:per-kwp])
      :style {:width :4em}
      :min 0 :max 1000}
     ]]
   [:td {:key :output}
    [inputs/number
     {:value-atom (reagent/cursor cost-atom [:per-kwh])
      :style {:width :4em}
      :min 0 :max 1000
      }]]))

(defn supply-tech-parameters [doc]
  (reagent/with-let [plant-options (reagent/cursor doc [::supply/plants])
                     storage-options (reagent/cursor doc [::supply/storages])
                     substations (reagent/cursor doc [::supply/substations])
                     fuel-options (reagent/track
                                   #(let [fuels (::supply/fuels @doc)]
                                      (sort (for [[k {n :name}] fuels]
                                              [k n]))))


                     focus-next (reagent/atom nil)
                     ]
    
    [:div.card.flex-grow.parameters-component
     [:h1 "Supply technologies"]
     [:div {:style {:overflow-x :auto}}
      [:table 
       [:thead
        [:tr
         [:th]
         [:th "Technology"]
         [:th "Lifetime"]
         [:th "Fuel"]
         [:th "CHP"]
         [:th "Capacity"]
         [:th "Power/fuel"]
         [:th "Heat/fuel"]
         [:th "Substation"]
         [:th {:col-span 3} "Capital cost"]
         [:th {:col-span 3} "Operating cost"]
         ]
        [:tr
         [:th ]
         [:th ]
         [:th "yr"]
         [:th ]
         [:th ]
         [:th "MW"]
         [:th "%"]
         [:th "%"]
         [:th ]
         [:th "k¤"]
         [:th "¤/kWp"]
         [:th "¤/kWh"]
         [:th "k¤"]
         [:th "¤/kWp"]
         [:th "¤/kWh"]
         ]
        ]
       [:tbody
        (doall
         (for [[plant-option params] (sort-by first @plant-options)]
           [:tr {:key plant-option}
            [:td [delete-button
                  {:on-click #(swap! doc supply/remove-plant plant-option)}]]
            [:td [inputs/text {:value-atom (reagent/cursor plant-options [plant-option :name])
                               :placeholder "Plant type"
                               :style {:width :8em}
                               :ref
                               (fn [e]
                                 (when (= [:plant plant-option]
                                          @focus-next)
                                   (reset! focus-next nil)
                                   (when e (.focus e))))}]
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
                   :style {:width :5em}
                   :step 0.1
                   :scale (/ 1000.0)
                   :min 1 :max 1000}]]
            [:td (if (:chp params)
                   [inputs/number
                    {:value-atom (reagent/cursor plant-options [plant-option :power-efficiency])
                     :style {:width :5em}
                     :scale 100
                     :step 0.1
                     :min 1 :max 100}]
                   [:em "n/a"]
                   )]
            [:td [inputs/number
                  {:value-atom (reagent/cursor plant-options [plant-option :heat-efficiency])
                   :style {:width :5em}
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
        ]
       ]]
     [:div {:style {:margin :1em}}
      [:button.button
       {:on-click
        #(let [ids (set (keys @plant-options))
               new-ids (-> (swap! doc supply/add-plant (new-plant ""))
                           (::supply/plants)
                           (keys)
                           (set))
               new-id (first (set/difference new-ids ids))]
           (when new-id
             (reset! focus-next [:plant new-id])))}
       "Add plant"
       ]]

     [:h1 "Substations"]
     [:div {:style {:overflow-x :auto}}
      [:table
       [:thead
        [:tr [:th] [:th "Name"] [:th "Headroom"] [:th "Alpha"]]
        [:tr [:th] [:th ] [:th "MW"] [:th "%"]]]
       
       [:tbody
        (for [[id params] (sort-by first @substations)]
          [:tr {:key id}
           [:td [delete-button {:on-click
                                #(swap! doc supply/remove-substation id)}]]
           [:td [inputs/text {:value-atom (reagent/cursor substations [id :name])
                              :placeholder "Name"
                              :ref (fn [e]
                                     (when (= [:substation id]
                                              @focus-next)
                                       (reset! focus-next nil)
                                       (when e (.focus e))))
                              
                              }]]
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
           ])]
       ]]
     [:div {:style {:margin :1em}}
      [:button.button
       {:on-click
        #(let [ids (set (keys @substations))
               new-ids (-> (swap! doc supply/add-substation (new-substation ""))
                           (::supply/substations)
                           (keys)
                           (set))
               new-id (first (set/difference new-ids ids))]
           (when new-id
             (reset! focus-next [:substation new-id])))}
       "Add substation"
       ]]
     
     
     [:h1 "Storage technologies"]
     [:div {:style {:overflow-x :auto}}
      [:table
       [:thead
        [:tr
         [:th]
         [:th "Name"]
         [:th "Lifetime"]
         [:th {:col-span 2} "Capacity"]
         [:th "Efficiency"]
         [:th {:col-span 3} "Capital cost"]
         ]
        [:tr
         [:th]
         [:th]
         [:th "yr"]
         [:th "MWh"]
         [:th "MW"]
         [:th "%"]
         [:th "k¤"] [:th "¤/kWp"] [:th "¤/kWh"]
         ]
        ]
       
       [:tbody
        (for [[id params] (sort-by first @storage-options)]
          [:tr {:key id}
           [:td [delete-button {:on-click #(swap! doc supply/remove-storage id)}]]
           [:td [inputs/text {:value-atom (reagent/cursor storage-options [id :name])
                              :placeholder "Name"
                              :ref (fn [e]
                                     (when (= [:storage id]
                                              @focus-next)
                                       (reset! focus-next nil)
                                       (when e (.focus e))))
                              }]]
           [:td [inputs/number {:value-atom (reagent/cursor storage-options [id :lifetime])
                                :style {:width :3em}
                                :min 1 :max 100}]]
           [:td [inputs/number
                 {:value-atom (reagent/cursor storage-options [id :capacity-kwh])
                  :step 0.1
                  :scale (/ 1000.0)
                  :style {:width :4em}
                  :min 0.1 :max 1000000}]]
           [:td [inputs/number
                 {:value-atom (reagent/cursor storage-options [id :capacity-kwp])
                  :scale (/ 1000.0)
                  :step 0.1
                  :style {:width :4em}
                  :min 0.1 :max 10000}]]
           [:td [inputs/number
                 {:value-atom (reagent/cursor storage-options [id :efficiency])
                  :style {:width :3em}
                  :scale 100
                  :step 0.1
                  :min 1 :max 100}]]
           (cost-cells
            (reagent/cursor storage-options [id :capital-cost]))
           ])]]
      
      [:div {:style {:margin :1em}}
       [:button.button
        {:on-click
         #(let [ids (set (keys @plant-options))
                new-ids (-> (swap! doc supply/add-storage (new-storage ""))
                            (::supply/storages)
                            (keys)
                            (set))
                new-id (first (set/difference new-ids ids))]
            (when new-id
              (reset! focus-next [:storage new-id])))}
        "Add storage"
        ]]
      ]
     
     
     ])
  )


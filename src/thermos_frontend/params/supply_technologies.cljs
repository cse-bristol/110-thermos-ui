;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.params.supply-technologies
  "Editor for supply problem technology definitions"
  (:require [reagent.core :as reagent]
            [re-frame.core :as rf]
            [thermos-frontend.doc :as doc]
            [thermos-frontend.inputs :as inputs]
            [thermos-frontend.debug-box :as debug]
            [thermos-specs.supply :as supply]
            [clojure.spec.alpha :as s]
            [thermos-pages.symbols :as syms]))

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

(defn cost-cells [cost-path]
  (list
   [:td {:key :fixed}
    [inputs/number
     {:path (conj cost-path :fixed)
      :style {:width :4em}
      :min 0 :max 1000 :scale (/ 1000.0)}]]
   [:td {:key :capacity}
    [inputs/number
     {:path (conj cost-path :per-kwp)
      :style {:width :4em}
      :min 0 :max 1000}
     ]]
   [:td {:key :output}
    [inputs/number
     {:path (conj cost-path :per-kwh)
      :style {:width :4em}
      :min 0 :max 1000
      }]]))

(defn- focus-if-new
  "A :ref for the name input of the `kind` of thing with `id`, which focuses
  it if it was just added. `focus-next` holds the kind of thing added and
  the ids there were before."
  [focus-next kind id]
  (fn [e]
    (let [[added-kind old-ids] @focus-next]
      (when (and e (= kind added-kind) (not (contains? old-ids id)))
        (reset! focus-next nil)
        (.focus e)))))

(defn supply-tech-parameters []
  (reagent/with-let [focus-next (reagent/atom nil)]
    (let [plant-options   (doc/value [::supply/plants])
          storage-options (doc/value [::supply/storages])
          substations     (doc/value [::supply/substations])
          fuel-options    (sort (for [[k {n :name}] (doc/value [::supply/fuels])]
                                  [k n]))
          add!            (fn [kind ids f new-thing]
                            (reset! focus-next [kind (set ids)])
                            (rf/dispatch [:doc/edit f new-thing]))]
    
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
         (for [[plant-option params] (sort-by first plant-options)]
           [:tr {:key plant-option}
            [:td [delete-button
                  {:on-click #(rf/dispatch [:doc/edit supply/remove-plant plant-option])}]]
            [:td [inputs/text {:path [::supply/plants plant-option :name]
                               :placeholder "Plant type"
                               :style {:width :8em}
                               :ref (focus-if-new focus-next :plant plant-option)}]
             ]
            [:td [inputs/number {:path [::supply/plants plant-option :lifetime]
                                 :style {:width :3em}
                                 :min 1 :max 100}]]
            [:td [inputs/select
                  {:path [::supply/plants plant-option :fuel]
                   :values fuel-options}]]
            [:td [inputs/check
                  {:on-change #(doc/update! [::supply/plants plant-option]
                                  (fn [x]
                                    (cond-> (assoc x :chp %)
                                      ;; make sure there is a power efficiency set if you turn on chp
                                      % (assoc :power-efficiency (or (:power-efficiency x) 0.75)))))
                   :value (-> params :chp)}]]
            [:td [inputs/number
                  {:path [::supply/plants plant-option :capacity-kwp]
                   :style {:width :5em}
                   :step 0.1
                   :scale (/ 1000.0)
                   :min 1 :max 1000}]]
            [:td (if (:chp params)
                   [inputs/number
                    {:path [::supply/plants plant-option :power-efficiency]
                     :style {:width :5em}
                     :scale 100
                     :step 0.1
                     :min 1 :max 100}]
                   [:em "n/a"]
                   )]
            [:td [inputs/number
                  {:path [::supply/plants plant-option :heat-efficiency]
                   :style {:width :5em}
                   :scale 100
                   :step 0.1
                   :min 1 :max 100}]]
            [:td
             [inputs/select
              {:path [::supply/plants plant-option :substation]
               :values (cons
                        [nil "None"]
                        (for [[id {name :name}] (sort-by first substations)]
                          [id name]))}]]
            
            (cost-cells
             [::supply/plants plant-option :capital-cost])
            
            (cost-cells
             [::supply/plants plant-option :operating-cost])
            ]))
        ]
       ]]
     [:div {:style {:margin :1em}}
      [:button.button
       {:on-click
        #(add! :plant (keys plant-options) supply/add-plant (new-plant ""))}
       "Add plant"
       ]]

     [:h1 "Substations"]
     [:div {:style {:overflow-x :auto}}
      [:table
       [:thead
        [:tr [:th] [:th "Name"] [:th "Headroom"] [:th "Alpha"]]
        [:tr [:th] [:th ] [:th "MW"] [:th "%"]]]
       
       [:tbody
        (for [[id params] (sort-by first substations)]
          [:tr {:key id}
           [:td [delete-button {:on-click
                                #(rf/dispatch [:doc/edit supply/remove-substation id])}]]
           [:td [inputs/text {:path [::supply/substations id :name]
                              :placeholder "Name"
                              :ref (focus-if-new focus-next :substation id)
                              
                              }]]
           [:td [inputs/number
                 {:path [::supply/substations id :headroom-kwp]
                  :scale (/ 1 1000.0)
                  :step 1
                  :min 1 :max 1000}]]
           [:td [inputs/number
                 {:path [::supply/substations id :alpha]
                  :scale 100
                  :step 0.1
                  :min 1 :max 100}]]
           ])]
       ]]
     [:div {:style {:margin :1em}}
      [:button.button
       {:on-click
        #(add! :substation (keys substations) supply/add-substation (new-substation ""))}
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
        (for [[id params] (sort-by first storage-options)]
          [:tr {:key id}
           [:td [delete-button {:on-click #(rf/dispatch [:doc/edit supply/remove-storage id])}]]
           [:td [inputs/text {:path [::supply/storages id :name]
                              :placeholder "Name"
                              :ref (focus-if-new focus-next :storage id)
                              }]]
           [:td [inputs/number {:path [::supply/storages id :lifetime]
                                :style {:width :3em}
                                :min 1 :max 100}]]
           [:td [inputs/number
                 {:path [::supply/storages id :capacity-kwh]
                  :step 0.1
                  :scale (/ 1000.0)
                  :style {:width :4em}
                  :min 0.1 :max 1000000}]]
           [:td [inputs/number
                 {:path [::supply/storages id :capacity-kwp]
                  :scale (/ 1000.0)
                  :step 0.1
                  :style {:width :4em}
                  :min 0.1 :max 10000}]]
           [:td [inputs/number
                 {:path [::supply/storages id :efficiency]
                  :style {:width :3em}
                  :scale 100
                  :step 0.1
                  :min 1 :max 100}]]
           (cost-cells
            [::supply/storages id :capital-cost])
           ])]]
      
      [:div {:style {:margin :1em}}
       [:button.button
        {:on-click
         #(add! :storage (keys storage-options) supply/add-storage (new-storage ""))}
        "Add storage"
        ]]
      ]
     
     
     ])))

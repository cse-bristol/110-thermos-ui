(ns thermos-frontend.candidate-editor
  (:require [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.tariff :as tariff]
            [thermos-specs.measure :as measure]
            
            [thermos-specs.path :as path]
            [thermos-specs.supply :as supply]
            [thermos-specs.document :as document]
            [thermos-frontend.popover :as popover]
            [thermos-frontend.editor-state :as state]
            [thermos-frontend.inputs :as inputs]
            [thermos-frontend.format :as format]
            
            [reagent.core :as reagent]
            [clojure.set :as set]))

;; TODO: something to help clear overrides of global parameters
;; applied in here

;; TODO: decision about what things ought to be in the db and what
;; should be scenario global.

(def group-by-options {::candidate/name "Name"
                       ::candidate/subtype "Classification"
                       ::candidate/type "Nothing"})

(def nil-value-label {::candidate/name "None"
                      ::candidate/subtype "Unclassified"})

(defn- demand-editor [tariffs connection-costs insulation alternatives state buildings]
  (reagent/with-let
    [group-by-key  (reagent/cursor state [:group-by])
     benchmarks    (reagent/cursor state [:benchmarks])
     values        (reagent/cursor state [:values])]
    
    [:div
     (when (seq (rest buildings))
       [:div
        [:h2 [:b "Edit buildings by "]
         [inputs/select 
          {:value-atom group-by-key
           :values group-by-options}]]])

     (let [group-by-key @group-by-key
           grouped (group-by group-by-key buildings)
           group-by-nothing (= group-by-key ::candidate/type)

           benchmarks @benchmarks
           demand-key (if benchmarks :demand-benchmark :demand)
           peak-key (if benchmarks :peak-benchmark :peak-demand)

           ;; this is a bit wrong still
           tariff-frequencies (frequencies (map ::tariff/id buildings))]
       [:table {:style {:max-height :400px :overflow-y :auto}}
        [:thead
         [:tr
          (when-not group-by-nothing
            [:th (group-by-options group-by-key)])
          [:th "Count"]
          [:th "Demand"]
          [:th "Peak"]
          [:th "Tariff"]
          [:th "Con. cost"]
          [:th "Insulation"]
          [:th "Alternatives"]
          [:th "Counterfactual"]]
         
         [:tr {:style {:font-size :small}}
          (when-not group-by-nothing [:th])
          [:th]
          [:th (if benchmarks "kWh/m2 yr" "MWh/yr")]
          [:th (if benchmarks "kW/m2" "kW")]
          [:th]
          [:th]
          [:th]
          [:th]]]
        
        [:tbody
         (doall
          (for [[k cands] grouped]
            [:tr {:key (or k "nil")}
             (when-not group-by-nothing
               [:td (or k (nil-value-label group-by-key))])
             [:td (count cands)]
             
             [:td [inputs/check-number
                   {:max 1000
                    :style {:max-width :5em}
                    ;; benchmarks are in kWh but absolute in MWh
                    :scale (if benchmarks 1 (/ 1 1000.0))
                    :min 0
                    :step 1
                    :value-atom
                    (reagent/cursor values [group-by-key k demand-key :value])
                    :check-atom
                    (reagent/cursor values [group-by-key k :demand :check])}]]
             [:td [inputs/check-number
                   {:max 1000
                    :style {:max-width :5em}
                    :min 0
                    :step 1
                    :value-atom
                    (reagent/cursor values [group-by-key k peak-key :value])
                    :check-atom
                    (reagent/cursor values [group-by-key k :peak-demand :check])}]]
             [:td [inputs/select
                   {:value-atom (reagent/cursor values [group-by-key k :tariff])
                    :values `[[:unset "Unchanged"]
                              ~@(map
                                 (fn [[id {name ::tariff/name}]] [id (str name " ("
                                                                          (get tariff-frequencies id 0 )
                                                                          ")")])
                                 tariffs)]}]]
             [:td [inputs/select
                   {:value-atom (reagent/cursor values [group-by-key k :connection-cost])
                    :values `[[:unset "Unchanged"]
                              ~@(map
                                 (fn [[id {name ::tariff/name}]] [id name ])
                                 connection-costs)]}]]

             [:td
              (doall
               (for [[id measure] insulation]
                 [:div {:key id}
                  [inputs/check
                   {:value (get-in @values [group-by-key k :insulation id])
                    :on-change #(swap! values
                                       assoc-in
                                       [group-by-key k :insulation id]
                                       %)
                    :label (::measure/name measure)}]]
                 ))]

             [:td
              (doall
               (for [[id alternative] alternatives]
                 [:div {:key id}
                  [inputs/check
                   {:value (get-in @values [group-by-key k :alternatives id])
                    :on-change #(swap! values
                                       assoc-in
                                       [group-by-key k :alternatives id]
                                       %)
                    :label (::supply/name alternative)}]]
                 ))
              ]
             [:td
              [inputs/select
               {:values `[[:unset "Unchanged"]
                          ~@(for [[i {n ::supply/name}] alternatives] [i n])]
                :value (get-in @values [group-by-key k :counterfactual])
                :on-change #(swap! values
                                   assoc-in
                                   [group-by-key k :counterfactual]
                                   %)}]]
             ]))]
        ])]))


(defn- path-editor [civils state paths]
  (reagent/with-let
    [group-by-key (reagent/cursor state [:group-by])
     values       (reagent/cursor state [:values])]
    [:div 
     (when (seq (rest paths))
       [:div
        [:h2 [:b "Edit paths by "]
         [inputs/select
          {:value-atom group-by-key
           :values group-by-options}]]])

     (let [group-by-key @group-by-key
           grouped (group-by group-by-key paths)]
       [:table {:style {:max-height :400px :overflow-y :auto}}
        [:thead
         [:tr
          [:th (group-by-options group-by-key)]
          [:th "Length (m)"]
          [:th.has-tt
           {:title "You can set up civil engineering costs in the pipe costs page."}
           "Civil cost"]]]
        
        [:tbody
         (doall
          (for [[k cands] grouped]
            [:tr {:key (or k "nil")}
             [:td (or k (nil-value-label group-by-key))]
             [:td (format/si-number (apply + (map ::path/length cands)))]
             [:td [inputs/select
                   {:value-atom (reagent/cursor values [group-by-key k :civil-cost])
                    :values `[[:unset "Unchanged"]
                              ~@(for [[id c] civils]
                                  [id (::path/civil-cost-name c)])]
                    }
                   ]]
             ]))]
        ])]))

(defn- mean [vals]
  (/ (reduce + vals) (float (count vals))))

(defn- unset? [vals]
  (if (apply = vals)
    (first vals)
    :unset))

(defn- initial-path-state [paths]
  {:group-by ::candidate/subtype
   :values
   (->> (for [k (keys group-by-options)]
          [k (->> paths (group-by k)
                  (map (fn [[k v]]
                         [k {:civil-cost
                             (unset? (map ::path/civil-cost-id v))}
                          ]))
                  (into {}))])
        (into {}))})

(defn- initial-building-state [buildings]
  {:group-by ::candidate/subtype
   :benchmarks false
   :values
   (->> (for [k (keys group-by-options)]
          [k
           (->> buildings
                (group-by k)
                (map (fn [[k v]]
                       (let [n (count v)]
                         [k (merge
                             {:demand {:value (mean (map ::demand/kwh v))}
                              :tariff (unset? (map ::tariff/id v))
                              :connection-cost (unset? (map ::tariff/cc-id v))
                              :peak-demand {:value (mean (map ::demand/kwp v))}
                              :demand-benchmark {:value 2}
                              :peak-benchmark {:value 3}
                              :insulation
                              (let [fs (frequencies
                                        (mapcat (comp seq ::demand/insulation) v))
                                    state
                                    (for [[i c] fs :when (pos? c)]
                                      [i
                                       (if (= n c) :true :indeterminate)])]
                                (into {} state))

                              :alternatives
                              (let [fs (frequencies
                                        (mapcat (comp seq ::demand/alternatives) v))
                                    state
                                    (for [[i c] fs :when (pos? c)]
                                      [i
                                       (if (= n c) :true :indeterminate)])]
                                (into {} state))

                              :counterfactual (unset? (map ::demand/counterfactual v))
                              
                              })])))
                
                (into {}))])
        (into {}))})

(defn- apply-path-state [document path-state path-ids]
  (let [{group :group-by values :values} path-state]

    (document/map-candidates
     document
     (fn [path]
       (let [path-group (group path)
             civil-cost-id (get-in values [group path-group :civil-cost])]
         (cond-> path
           (not= :unset civil-cost-id)
           (assoc ::path/civil-cost-id civil-cost-id
                  ::candidate/modified true))))
     path-ids)))

(defn- apply-building-state [document building-state building-ids]
  (let [{group :group-by
         bench :benchmarks
         values :values}
        building-state]
    ;; we need to update the candidates in the document
    ;; modifying each one to have the relevant group-by demand-value
    (document/map-candidates
     document
     (fn [building]
       (let [building-group (group building)

             values (get-in values [group building-group])

             {set-peak :check peak-value :value} (:peak-demand values)
             {set-demand :check demand-value :value} (:demand values)

             peak-value   (if bench
                            (* (:peak-benchmark values) (::candidate/area building))
                            peak-value)

             demand-value (if bench
                            (* (:demand-benchmark values) (::candidate/area building))
                            demand-value)

             tariff (:tariff values)

             set-tariff (not= :unset tariff)

             connection-cost (:connection-cost values)

             set-connection-cost (not= :unset connection-cost)
             
             counterfactual (:counterfactual values)

             set-counterfactual (not= :unset counterfactual)

             remove-insulation
             (->> (:insulation values)
                  (filter #(not (second %)))
                  (map first)
                  (set))
             
             add-insulation
             (->> (:insulation values)
                  (filter #(= true (second %)))
                  (map first)
                  (set))

             remove-alternatives
             (->> (:alternatives values)
                  (filter #(not (second %)))
                  (map first)
                  (set))
             
             add-alternatives
             (->> (:alternatives values)
                  (filter #(= true (second %)))
                  (map first)
                  (set))
             ]

         (cond-> building
           set-demand              (assoc ::demand/kwh demand-value)
           set-peak                (assoc ::demand/kwp peak-value)
           set-tariff              (assoc ::tariff/id tariff)
           set-connection-cost     (assoc ::tariff/cc-id connection-cost)
           set-counterfactual      (assoc ::demand/counterfactual counterfactual)
           
           (seq add-insulation)    (update ::demand/insulation set/union add-insulation)
           (seq remove-insulation) (update ::demand/insulation set/difference remove-insulation)
           (seq add-alternatives)    (update ::demand/alternatives set/union add-alternatives)
           (seq remove-alternatives) (update ::demand/alternatives set/difference remove-alternatives)
           

           ;; ensure we don't delete it since we edited it
           (or set-demand
               set-peak
               set-tariff
               set-connection-cost
               set-counterfactual
               
               (seq add-insulation)
               (seq remove-insulation)
               (seq add-alternatives)
               (seq remove-alternatives))
           (assoc ::candidate/modified true)
           )))

     building-ids)))

(defn- candidate-editor [document candidate-ids]
  (reagent/with-let [candidates (map (::document/candidates @document) candidate-ids)
                     {buildings :building paths :path}
                     (group-by ::candidate/type candidates)

                     demand-state (reagent/atom (initial-building-state buildings))
                     path-state   (reagent/atom (initial-path-state paths))
                     tariffs (sort-by first (::document/tariffs @document))
                     connection-costs (sort-by first (::document/connection-costs @document))
                     civils (sort-by first (::document/civil-costs @document))
                     insulation (sort-by first (::document/insulation @document))
                     alternatives (sort-by first (::document/alternatives @document))
                     ]
    
    [:div.popover-dialog
     (when (seq buildings)
       [demand-editor
        tariffs
        connection-costs
        insulation
        alternatives
        
        demand-state buildings])
     (when (seq paths)
       [:div
        (when (seq buildings) [:hr])
        [path-editor civils path-state paths]])
     [:div
      [:button.button.button--danger
       {:on-click popover/close!
        :style {:margin-left :auto}}
       "Cancel"]
      [:button.button
       {:on-click (fn []
                    (state/edit! document
                                 #(-> %
                                      (apply-path-state @path-state
                                                        (map ::candidate/id paths)
                                                        )
                                      (apply-building-state @demand-state
                                                            (map ::candidate/id buildings)
                                                            )))
                    (popover/close!))}
       "OK"]]]
    ))



(defn show-editor! [document candidate-ids]
  (when (seq candidate-ids)
    (popover/open! [candidate-editor document candidate-ids]
                   :middle)))

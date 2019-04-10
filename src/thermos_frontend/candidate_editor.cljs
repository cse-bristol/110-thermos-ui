(ns thermos-frontend.candidate-editor
  (:require [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.path :as path]
            [thermos-specs.document :as document]
            [thermos-frontend.popover :as popover]
            [thermos-frontend.editor-state :as state]
            [thermos-frontend.inputs :as inputs]
            [thermos-frontend.format :as format]
            [reagent.core :as reagent]))

;; TODO: something to help clear overrides of global parameters
;; applied in here

;; TODO: decision about what things ought to be in the db and what
;; should be scenario global.

(def group-by-options {::candidate/name "Name"
                       ::candidate/subtype "Classification"
                       ::candidate/type "Nothing"})

(def nil-value-label {::candidate/name "None"
                      ::candidate/subtype "Unclassified"})

(defn- demand-editor [state buildings]
  (reagent/with-let
    [group-by-key  (reagent/cursor state [:group-by])
     benchmarks    (reagent/cursor state [:benchmarks])
     values        (reagent/cursor state [:values])
     ]
    [:div
     (when (seq (rest buildings))
       [:div
        [:h2 "Group by "
         [inputs/select 
          {:value-atom group-by-key
           :values group-by-options}]]])

     (let [group-by-key @group-by-key
           grouped (group-by group-by-key buildings)
           group-by-nothing (= group-by-key ::candidate/type)

           benchmarks @benchmarks
           demand-key (if benchmarks :demand-benchmark :demand)
           peak-key (if benchmarks :peak-benchmark :peak-demand)
           ]
       [:table {:style {:max-height :400px :overflow-y :auto}}
        [:thead
         [:tr
          (when-not group-by-nothing
            [:th (group-by-options group-by-key)])
          [:th "Count"]
          [:th "Connection cost"]
          [:th "Demand"]
          [:th "Peak"]
          [:th "Heat price"]
          (for [e candidate/emissions-types]
            [:th {:key e} (name e)])
          ]
         [:tr {:style {:font-size :small}}
          (when-not group-by-nothing [:th])
          [:th]
          [:th "¤/kWp"]
          [:th (if benchmarks "kWh/m2 yr" "MWh/yr")]
          [:th (if benchmarks "kW/m2" "kW")]
          [:th "c/kWh"]
          (for [e candidate/emissions-types]
            [:th {:key e} "kg/kWh"])
          ]
         
         ]
        [:tbody
         (doall
          (for [[k cands] grouped]
            [:tr {:key (or k "nil")}
             (when-not group-by-nothing
               [:td (or k (nil-value-label group-by-key))])
             [:td (count cands)]
             [:td [inputs/check-number
                   {:max 1000
                    :min 0
                    :step 1
                    :value-atom
                    (reagent/cursor values [group-by-key k :connection-cost :value])
                    :check-atom
                    (reagent/cursor values [group-by-key k :connection-cost :check])}]]
             [:td [inputs/check-number
                   {:max 1000
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
                    :min 0
                    :step 1
                    :value-atom
                    (reagent/cursor values [group-by-key k peak-key :value])
                    :check-atom
                    (reagent/cursor values [group-by-key k :peak-demand :check])}]]
             [:td [inputs/check-number
                   {:scale 100 ;; store in £ not p
                    :step 0.1
                    :max 100
                    :min 0
                    :value-atom
                    (reagent/cursor values [group-by-key k :price :value])
                    :check-atom
                    (reagent/cursor values [group-by-key k :price :check])}]]
             (doall
              (for [e candidate/emissions-types]
                [:th {:key e}
                 [inputs/check-number
                  {:step 0.1
                   :max 1000
                   :min 0
                   :value-atom
                   (reagent/cursor values [group-by-key k e :value])
                   :check-atom
                   (reagent/cursor values [group-by-key k e :check])
                   }]]))
             ]))]
        ])]))


(defn- path-editor [state paths]
  (reagent/with-let
    [group-by-key (reagent/cursor state [:group-by])
     values       (reagent/cursor state [:values])]
    [:div 
     (when (seq (rest paths))
       [:div
        [:h2 "Group by "
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
          [:th
           {:title "This is the fixed part of the civil engineering cost."}
           "Civil cost (¤/m)"]
          [:th
           {:title (str "This is the variable part of the civil engineering cost. "
                        "The actual cost will be length * (fixed cost + (variable cost * diameter)^1.1)")}
           
           "Civil cost (¤/~m2)"]]]
        
        [:tbody
         (doall
          (for [[k cands] grouped]
            [:tr {:key (or k "nil")}
             [:td (or k (nil-value-label group-by-key))]
             [:td (format/si-number (apply + (map ::path/length cands)))]
             [:td [inputs/check-number
                   {:value-atom
                    (reagent/cursor values [group-by-key k :cost-per-m :value])
                    :check-atom
                    (reagent/cursor values [group-by-key k :cost-per-m :check])}
                   ]]

             [:td [inputs/check-number
                   {:value-atom
                    (reagent/cursor values [group-by-key k :cost-per-m2 :value])
                    :check-atom
                    (reagent/cursor values [group-by-key k :cost-per-m2 :check])}
                   ]]
             ]))]
        ])]))

(defn- mean [vals]
  (/ (reduce + vals) (float (count vals))))

(defn- initial-path-state [paths]
  {:group-by ::candidate/subtype
   :values
   (->> (for [k (keys group-by-options)]
          [k (->> paths (group-by k)
                  (map (fn [[k v]]
                         [k {:cost-per-m {:value (mean (map ::path/cost-per-m v))}
                             :cost-per-m2 {:value (mean (map ::path/cost-per-m2 v))}
                             }]))
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
                       [k (merge
                           {:demand {:value (mean (map ::demand/kwh v))}
                            :peak-demand {:value (mean (map ::demand/kwp v))}
                            :demand-benchmark {:value 2}
                            :peak-benchmark {:value 3}
                            :price {:value (mean (map ::demand/price v))}
                            :connection-cost {:value (mean (map ::demand/connection-cost v))}}
                           (->> (for [e candidate/emissions-types]
                                  [e {:value
                                      (mean
                                       (map
                                        #(get-in % [::demand/emissions e])
                                        v))
                                      }])
                                (into {}))
                           )]))
                (into {}))])
        (into {}))})

(defn- apply-path-state [document path-state path-ids]
  (let [{group :group-by values :values} path-state]

    (document/map-candidates
     document
     (fn [path]
       (let [path-group (group path)
             {f-cost :value f-set-cost :check} (get-in values [group path-group :cost-per-m])
             {v-cost :value v-set-cost :check} (get-in values [group path-group :cost-per-m2])
             ]
         (cond-> path
           f-set-cost (assoc ::path/cost-per-m f-cost)
           v-set-cost (assoc ::path/cost-per-m2 v-cost))))
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
             {set-conn-cost :check conn-cost-value :value} (:connection-cost values)

             peak-value   (if bench
                            (* (:peak-benchmark values) (::candidate/area building))
                            peak-value)

             demand-value (if bench
                            (* (:demand-benchmark values) (::candidate/area building))
                            demand-value)

             {set-heat-price :check price-value :value} (:price values)

             emissions-factors
             (select-keys values candidate/emissions-types)

             emissions-factors
             (->> (for [[e {c :check v :value}] emissions-factors
                        :when c]
                    [e v])
                  (into {}))]
         
         (cond-> building
           set-demand              (assoc ::demand/kwh demand-value)
           set-peak                (assoc ::demand/kwp peak-value)
           set-heat-price          (assoc ::demand/price price-value)
           set-conn-cost           (assoc ::demand/connection-cost conn-cost-value)
           (seq emissions-factors) (update ::demand/emissions merge emissions-factors))))

     building-ids)))

(defn- candidate-editor [document candidate-ids]
  (reagent/with-let [candidates (map (::document/candidates @document) candidate-ids)
                     {buildings :building paths :path}
                     (group-by ::candidate/type candidates)

                     demand-state (reagent/atom (initial-building-state buildings))
                     path-state   (reagent/atom (initial-path-state paths))
                     ]
    [:div.popover-dialog
     (when (seq buildings)
       [demand-editor demand-state buildings])
     (when (seq paths)
       [path-editor path-state paths])
     [:div
      [:button {:on-click popover/close!}
       "Cancel"]
      [:button {:on-click (fn []
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

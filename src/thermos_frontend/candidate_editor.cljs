;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.candidate-editor
  (:require [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.cooling :as cooling]
            [thermos-specs.tariff :as tariff]
            [thermos-specs.measure :as measure]

            [thermos-specs.path :as path]
            [thermos-specs.supply :as supply]
            [thermos-specs.document :as document]
            [thermos-frontend.popover :as popover]
            [thermos-frontend.editor-state :as state]
            [thermos-frontend.inputs :as inputs]
            [thermos-frontend.format :as format]
            [thermos-frontend.util :refer [target-value]]

            [thermos-util.peak-demand :as peak]
            [thermos-util :refer [annual-kwh->kw]]
            
            [clojure.string :as string]

            [reagent.core :as reagent]
            [clojure.set :as set]))

(defn- nil-value-label [k] "None")

(defn- mean [vals]
  (if (empty? vals)
    0
    (/ (reduce + vals) (float (count vals)))))

(defn- unset? [vals]
  (if (apply = vals)
    (first vals)
    :unset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TARIFFS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- tariff-editor [group-by-options tariffs connection-costs state buildings]
  (reagent/with-let
    [group-by-key  (reagent/cursor state [:group-by])
     values        (reagent/cursor state [:values])]

    [:div
     (when (seq (rest buildings))
       [:div
        [:div [:b "Edit tariffs by "]
         [inputs/select
          {:value-atom group-by-key
           :values group-by-options}]]])

     (let [group-by-key @group-by-key
           grouped (group-by group-by-key buildings)
           group-by-nothing (= group-by-key ::candidate/type)

           tariff-frequencies (frequencies (map ::tariff/id buildings))]
       [:table.table.table--alternating {:style {:max-height :400px :overflow-y :auto :margin-top :1em}}
        [:thead
         [:tr
          (when-not group-by-nothing
            [:th (group-by-options group-by-key)])
          [:th.align-right "Count"]
          
          [:th "Tariff"]
          [:th "Con. cost"]
          ]]

        [:tbody
         (doall
          (for [[k cands] grouped]
            
            (list
             [:tr {:key (or k "nil")}
              (when-not group-by-nothing
                [:td (or k (nil-value-label group-by-key))])
              [:td.align-right (count cands)]
              
              
              [:td [inputs/select
                    {:value-atom (reagent/cursor values [group-by-key k :tariff])
                     :values `[[:unset "Unchanged"]
                               [:market "Market"]
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
              
              ])))]
        ])]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INSULATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn- insulation-editor [group-by-options insulation alternatives state buildings]
  (reagent/with-let
    [group-by-key  (reagent/cursor state [:group-by])
     values        (reagent/cursor state [:values])]

    [:div
     (when (seq (rest buildings))
       [:div
        [:div [:b "Edit other technologies by "]
         [inputs/select
          {:value-atom group-by-key
           :values group-by-options}]]])

     (let [group-by-key @group-by-key
           grouped (group-by group-by-key buildings)
           group-by-nothing (= group-by-key ::candidate/type)]
       
       [:table.table.table--alternating {:style {:max-height :400px :overflow-y :auto :margin-top :1em}}
        [:thead
         [:tr
          (when-not group-by-nothing
            [:th (group-by-options group-by-key)])
          [:th.align-right "Count"]
          
          [:th "Insulation"]
          [:th "Alternatives"]
          [:th "Counterfactual"]]]

        [:tbody
         (doall
          (for [[k cands] grouped]
            
            (list
             [:tr {:key (or k "nil")}
              (when-not group-by-nothing
                [:td (or k (nil-value-label group-by-key))])
              [:td.align-right (count cands)]

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
                           [:nothing "Nothing"]
                           ~@(for [[i {n ::supply/name}] alternatives] [i n])]
                 :value (get-in @values [group-by-key k :counterfactual])
                 :on-change #(swap! values
                                    assoc-in
                                    [group-by-key k :counterfactual]
                                    %)}]]
              ])))]
        ])]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DEMAND EDITOR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn- demand-editor [group-by-options profiles state buildings]
  (reagent/with-let
    [group-by-key  (reagent/cursor state [:group-by])
     *demand-key   (reagent/cursor state [:demand-key])
     *peak-key     (reagent/cursor state [:peak-key])
     values        (reagent/cursor state [:values])]

    [:div
     (when (seq (rest buildings))
       [:div
        [:div

         [:b "Edit demands by "]
         [inputs/select
          {:value-atom group-by-key
           :values group-by-options}]
         ]])

     (let [group-by-key @group-by-key
           grouped (group-by group-by-key buildings)
           group-by-nothing (= group-by-key ::candidate/type)
           demand-key @*demand-key
           peak-key @*peak-key
           ]
       
       [:table.table.table--alternating {:style {:max-height :400px :overflow-y :auto :margin-top :1em}}
        [:thead
         [:tr
          (when-not group-by-nothing
            [:th (group-by-options group-by-key)])
          [:th.align-right "Count"]
          
          [:th "Connections"]
          [:th {:style {:min-width :140px}}
           [inputs/select
                {:values [[:demand "Demand (MWh/yr)"]
                          [:demand-factor "Demand (%)"]]
                 :value demand-key
                 :on-change #(reset! *demand-key %)}
                ]
           ]
          [:th [inputs/select
                {:values [[:peak-demand "Peak (kW)"]
                          [:peak-factor "Peak (%)"]
                          [:peak-estimate "Peak (re-estimate)"]]
                 :value :peak-key
                 :on-change #(reset! *peak-key %)}
                ]
           ]
          [:th.has-tt {:title "This affects the supply model, not the network model."} "Profile"]
          ]
         
         ]

        [:tbody
         (doall
          (for [[k cands] grouped]
            
            (list
             [:tr {:key (or k "nil")}
              (when-not group-by-nothing
                [:td (or k (nil-value-label group-by-key))])
              [:td.align-right (count cands)]

              [:td [inputs/check-number
                    {:key :connection-count
                     :max 1000
                     :min 1
                     :step 1
                     :style {:max-width :5em}
                     :value-atom
                     (reagent/cursor values [group-by-key k :connection-count :value])
                     :check-atom
                     (reagent/cursor values [group-by-key k :connection-count :check])
                     }
                    ]]
              
              [:td [inputs/check-number
                    {:key demand-key
                     :max 1000
                     :style {:max-width :5em}
                     ;; benchmarks are in kWh but absolute in MWh
                     :scale (case demand-key
                              :demand (/ 1 1000.0)
                              :demand-benchmark 1
                              :demand-factor 100.0 ;; -percent
                              )
                     :min 0
                     :step 1
                     :value-atom
                     (reagent/cursor values [group-by-key k demand-key :value])
                     :check-atom
                     (reagent/cursor values [group-by-key k :demand :check])}]]
              [:td
               (if (= peak-key :peak-estimate)
                 [inputs/check
                  {:key peak-key
                   :value (get-in @values
                                  [group-by-key k :peak-demand :check])
                   :on-change #(swap! values
                                     assoc-in
                                     [group-by-key k :peak-demand :check]
                                     %)}]
                 
                 [inputs/check-number
                  {:key peak-key
                   :max 1000
                   :scale (case peak-key
                            :peak-demand 1
                            :peak-benchmark 1
                            :peak-factor 100.0
                            )
                   :style {:max-width :5em}
                   :min 0
                   :step 1
                   :value-atom
                   (reagent/cursor values [group-by-key k peak-key :value])
                   :check-atom
                   (reagent/cursor values [group-by-key k :peak-demand :check])}])]

              [:td
               ;; profile choices go here.
               [inputs/select
                {:values `[[:unset "Unchanged"] ~@profiles]
                 :value (get-in @values [group-by-key k :profile])
                 :on-change #(swap! values
                                    assoc-in
                                    [group-by-key k :profile]
                                    %)}]
               ]

              
              ])))]
        ])]))


(defn- initial-building-state [group-by-options buildings mode]
  (let [[annual-demand peak-demand]
        (case mode
          :cooling [::cooling/kwh ::cooling/kwp]
          [::demand/kwh ::demand/kwp])]
    {:group-by   ::candidate/type
     :demand-key :demand
     :peak-key   :peak-demand
     :values
     (->> (for [k (keys group-by-options)]
            [k
             (->> buildings
                  (group-by k)
                  (map (fn [[k v]]
                         (let [n (count v)]
                           [k (merge
                               {:demand           {:value (mean (map annual-demand v))}
                                :tariff           (unset? (map ::tariff/id v))
                                :connection-count {:value (int (mean (map ::demand/connection-count v)))}
                                :profile          (unset? (map ::supply/profile-id v))
                                :connection-cost  (unset? (map ::tariff/cc-id v))
                                :peak-demand      {:value (mean (map peak-demand v))}
                                :demand-benchmark {:value 2}
                                :peak-benchmark   {:value 3}
                                :demand-factor    {:value 1}
                                :peak-factor      {:value 1}
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

                                :counterfactual (or (unset? (map ::demand/counterfactual v))
                                                    :nothing)

                                })])))

                  (into {}))])
          (into {}))}))

(defn- apply-demand-state [document demand-state building-ids]
  (let [{group :group-by
         demand-key :demand-key
         peak-key   :peak-key
         values :values}
        demand-state


        [annual-demand peak-demand]
        (if (document/is-cooling? document)
          [::cooling/kwh ::cooling/kwp]
          [::demand/kwh ::demand/kwp])
        ]
    
    ;; we need to update the candidates in the document
    ;; modifying each one to have the relevant group-by demand-value
    (document/map-candidates
     document
     (fn [building]
       (let [building-group (group building)

             values (get-in values [group building-group])

             {set-peak :check peak-value :value} (:peak-demand values)
             {set-demand :check demand-value :value} (:demand values)
             {set-cc :check cc-value :value} (:connection-count values)
             
             demand-value (if set-demand
                            (case demand-key
                              :demand           demand-value
                              :demand-benchmark (* (:value (:demand-benchmark values)) (::candidate/area building))
                              :demand-factor    (* (:value (:demand-factor values)) (annual-demand building)))
                            
                            (annual-demand building))
             
             peak-value   (case peak-key
                            :peak-demand     peak-value
                            :peak-benchmark  (* (:value (:peak-benchmark values)) (::candidate/area building))
                            :peak-factor     (* (:value (:peak-factor values)) (peak-demand building))
                            :peak-estimate   (peak/annual->peak-demand demand-value))
             
             ;; ensure peak is consistent
             min-peak    (annual-kwh->kw demand-value)

             set-peak    (or set-peak (> min-peak peak-value))
             peak-value  (max min-peak peak-value)
             
             profile (:profile values)
             set-profile (not= :unset profile)]
         (cond-> building
           set-demand              (assoc annual-demand demand-value)
           set-peak                (assoc peak-demand peak-value)
           set-profile             (assoc ::supply/profile-id profile)
           set-cc                  (assoc ::demand/connection-count cc-value)

           ;; ensure we don't delete it since we edited it
           (or set-demand
               set-peak
               set-profile
               set-cc)
           (assoc ::candidate/modified true))))
     building-ids)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PATH EDITOR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- initial-path-state [group-by-options paths]
  {:group-by ::candidate/type
   :values
   (->> (for [k (keys group-by-options)]
          [k (->> paths (group-by k)
                  (map (fn [[k v]]
                         [k {:max-diameter
                             {:value (when-let [vals (seq (keep ::path/maximum-diameter v))]
                                       (mean vals))
                              :check false}
                             :civil-cost
                             (unset? (map ::path/civil-cost-id v))
                             :exists
                             (let [exists (keep ::path/exists v)]
                               (cond
                                 (empty? exists) false
                                 (= (count exists)
                                    (count v)) true
                                 :else :indeterminate))}
                          
                          ]))
                  (into {}))])
        (into {}))})

(defn- apply-path-state [document path-state path-ids]
  (let [{group :group-by values :values} path-state]

    (document/map-candidates
     document
     (fn [path]
       (let [path-group (group path)
             civil-cost-id (get-in values [group path-group :civil-cost])
             exists (get-in values [group path-group :exists])

             {max-dia :value check-dia :check}
             (get-in values [group path-group :max-diameter])
             ]
         (cond-> path
           (not= :unset civil-cost-id)
           (assoc ::path/civil-cost-id civil-cost-id
                  ::candidate/modified true)

           (= true exists)
           (assoc ::path/exists true)

           (= false exists)
           (dissoc ::path/exists)
           
           (and check-dia
                (number? max-dia))
           (assoc ::path/maximum-diameter max-dia)

           (and check-dia
                (nil? max-dia))
           (dissoc ::path/maximum-diameter))))
     path-ids)))

(defn- path-editor [group-by-options civils min-pipe-diameter state paths]
  (reagent/with-let
    [group-by-key (reagent/cursor state [:group-by])
     values       (reagent/cursor state [:values])]
    [:div
     (when (seq (rest paths))
       [:div
        [:div [:b "Edit paths by "]
         [inputs/select
          {:value-atom group-by-key
           :values group-by-options}]]])

     (let [group-by-key @group-by-key
           grouped (group-by group-by-key paths)]
       [:table.table.table--alternating {:style {:max-height :400px :overflow-y :auto}}
        [:thead
         [:tr
          [:th {:style {:width :100%}} (group-by-options group-by-key)]
          [:th.align-right {:style {:min-width :120px :padding-right :25px}} "Length (m)"]
          [:th.align-right [:span.has-tt
                            {:title "If exists is checked, this will set the resulting pipe diameter."}
                            "Maximum diameter (mm)"]]
          [:th.align-right "Exists"]
          [:th.has-tt
           {:title "You can set up civil engineering costs in the pipe costs page."}
           "Civil cost"]]]

        [:tbody
         (doall
          (for [[k cands] grouped]
            [:tr {:key (or k "nil")}
             [:td (or k (nil-value-label group-by-key))]
             [:td.align-right {:style {:padding-right :25px}} (format/si-number (apply + (map ::path/length cands)))]
             [:td [inputs/check-number
                   {:max 5000
                    :min min-pipe-diameter
                    :step 1
                    :scale 1000.0
                    :empty-value [nil "∞"]
                    :value-atom (reagent/cursor values [group-by-key k :max-diameter :value])
                    :check-atom (reagent/cursor values [group-by-key k :max-diameter :check])}]]
             
             [:td [inputs/check {:value (get-in @values [group-by-key k :exists])
                                 :on-change #(swap! values assoc-in [group-by-key k :exists] %)
                                 }]]
             [:td [inputs/select
                   {:value-atom (reagent/cursor values [group-by-key k :civil-cost])
                    :values `[[:unset "Unchanged"] ~@civils]
                    }
                   ]]
             ]))]
        ])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; USER DEFINED FIELD EDITOR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- initial-category-state [group-by-options candidates]
  {:group-by ::candidate/type
   :user-fields []
   :values
   (->> (for [k (keys group-by-options)]
          [k { ;; {(k candidate) => {index in user-fields => value}}
              }])
        (into {}))})

(defn- apply-category-state  [document category-state candidate-ids]
  (let [{group :group-by values :values
         user-fields :user-fields} category-state
        values (get values group)]
    (document/map-candidates
     document

     (fn [candidate]
       (let [group (group candidate)
             new-values (get values group)]
         (reduce-kv
          (fn [candidate k v]
            (let [field (some-> (nth user-fields k) (string/trim))
                  v     (some-> v (string/trim))
                  cur   (get (::candidate/user-fields candidate) field)]
              (if (or (string/blank? field)
                      (and (string/blank? v)
                           (string/blank? cur)))
                candidate
                (-> candidate
                    (update ::candidate/user-fields assoc field v)
                    (assoc ::candidate/modified true)))))
          candidate
          new-values)))

     candidate-ids)))

(defn- category-editor [group-by-options candidates state]
  (reagent/with-let [group-by-key (reagent/cursor state [:group-by])
                     *values (reagent/cursor state [:values])
                     group-by-options (assoc group-by-options
                                             ::candidate/type
                                             "Type")
                     user-fields (sort (vals (dissoc group-by-options ::candidate/type)))

                     columns (reagent/cursor state [:user-fields])
                     ]
    [:div
     [:datalist#user-fields
      (for [f user-fields] [:option {:key f :value f}])]
     
     (when (seq (rest candidates))
       [:div [:b "Edit candidates by "]
        [inputs/select {:value-atom group-by-key :values group-by-options
                        }]])

     (let [group-by-key @group-by-key]
       [:table.table.table--alternating {:style {:max-height :400px :overflow-y :auto}}
        [:thead
         [:tr
          [:th (group-by-options group-by-key)]
          (doall
           (for [[i v] (map-indexed vector @columns)]
             [:th {:key i}
              [:input.input {:list :user-fields
                             :type :text
                             :style {:width :100%}
                             :value v
                             :on-change #(swap! columns assoc i (-> % .-target .-value))
                             }]
              ]
             ))
          [:th [:button
                {:on-click #(swap! columns conj "")}
                "+"]]
          ]]
        
        [:tbody
         (let [values @*values
               values (get values group-by-key)
               columns @columns]
           (for [[k cands] (group-by group-by-key candidates)]
             [:tr {:key (or k "nil")}
              [:td (or k (nil-value-label group-by-key))]
              (for [[i v] (map-indexed vector columns)]
                [:th {:key i}
                 [:input.input
                  {:type :text
                   :style {:width :100%}
                   :value (-> values (get k) (get i))
                   :on-change #(swap! *values assoc-in
                                      [group-by-key k i]
                                      (-> % .-target .-value))
                   }]])
              [:td]]
             ))]])]))




(defn- apply-tariff-state [document tariff-state building-ids]
  (let [{group :group-by
         bench :benchmarks
         values :values}
        tariff-state]
    ;; we need to update the candidates in the document
    ;; modifying each one to have the relevant group-by demand-value
    (document/map-candidates
     document
     (fn [building]
       (let [building-group (group building)

             values (get-in values [group building-group])

             tariff (:tariff values)
             set-tariff (not= :unset tariff)

             connection-cost (:connection-cost values)
             set-connection-cost (not= :unset connection-cost)
             ]
         (cond-> building
           set-tariff              (assoc ::tariff/id tariff)
           set-connection-cost     (assoc ::tariff/cc-id connection-cost)
           ;; ensure we don't delete it since we edited it
           (or set-tariff set-connection-cost)
           (assoc ::candidate/modified true)
           )))
     building-ids)))

(defn- apply-technology-state [document technology-state building-ids]
  (let [{group :group-by
         bench :benchmarks
         values :values}
        technology-state
        ]
    
    ;; we need to update the candidates in the document
    ;; modifying each one to have the relevant group-by demand-value
    (document/map-candidates
     document
     (fn [building]
       (let [building-group (group building)

             values (get-in values [group building-group])

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
           (and set-counterfactual
                (not= :nothing counterfactual))
           (assoc ::demand/counterfactual counterfactual)
           
           (and set-counterfactual
                (= :nothing counterfactual))
           (dissoc ::demand/counterfactual)

           (seq add-insulation)    (update ::demand/insulation set/union add-insulation)
           (seq remove-insulation) (update ::demand/insulation set/difference remove-insulation)
           (seq add-alternatives)    (update ::demand/alternatives set/union add-alternatives)
           (seq remove-alternatives) (update ::demand/alternatives set/difference remove-alternatives)

           ;; ensure we don't delete it since we edited it
           (or set-counterfactual

               (seq add-insulation)
               (seq remove-insulation)
               (seq add-alternatives)
               (seq remove-alternatives))
           (assoc ::candidate/modified true)
           )))

     building-ids)))

(defn- get-group-by-options [candidates]
  (->
   (set (mapcat (comp keys ::candidate/user-fields) candidates))
   (->> (map (fn [k] [#(-> % ::candidate/user-fields (get k)) k])) (into {}))
   (assoc ::candidate/type "Nothing")))

(defn- candidate-editor [document candidate-ids]
  (reagent/with-let [mode (document/mode @document)

                     candidates (map (::document/candidates @document) candidate-ids)
                     {buildings :building paths :path}
                     (group-by ::candidate/type candidates)

                     building-groups (get-group-by-options buildings)
                     path-groups (get-group-by-options paths)
                     all-groups (get-group-by-options candidates)

                     bstate (initial-building-state building-groups buildings mode)
                     demand-state (reagent/atom bstate)
                     tariff-state (reagent/atom bstate)
                     technology-state (reagent/atom bstate)
                     
                     path-state   (reagent/atom (initial-path-state path-groups paths))
                     tariffs (sort-by first (::document/tariffs @document))

                     profiles
                     (for [[id {n :name}]
                           (sort-by first (::supply/heat-profiles @document))]
                       [id n])
                     
                     connection-costs (sort-by first (::document/connection-costs @document))
                     civils (sort-by first (:civils (::document/pipe-costs @document)))
                     insulation (sort-by first (::document/insulation @document))
                     alternatives (sort-by first (::document/alternatives @document))
                     min-pipe-diameter (* 1000
                                          (::document/minimum-pipe-diameter document 0.02))

                     category-state (reagent/atom (initial-category-state all-groups
                                                                          candidates))
                     ]

    [:div.popover-dialog.popover-dialog--wide
     [:h2.popover-header "Edit Candidates"]
     [:button.popover-close-button
      {:on-click popover/close!}
      "⨯"]

     (let [tabs
           (remove
            nil?

            `[~@(when (seq buildings)
                  [[:demands "Demands"
                    [demand-editor
                     building-groups
                     profiles
                     demand-state
                     buildings]]
                   [:tariff "Tariff & Connection Costs"
                    [tariff-editor
                     building-groups
                     tariffs
                     connection-costs
                     tariff-state
                     buildings]
                    ]
                   [:insulation "Insulation & Systems"
                    [insulation-editor
                     building-groups
                     insulation
                     alternatives
                     technology-state
                     buildings
                     ]
                    ]
                   ])
              
              ~(when (seq paths)
                 [:paths "Paths"
                  [path-editor path-groups civils min-pipe-diameter path-state paths]])

              ~[:category "Other Fields"
                [category-editor all-groups candidates category-state]
                ]
              ]
            )]
       (if (seq (rest tabs))
         (reagent/with-let [active-tab (reagent/atom (first (first tabs)))]
           [:div
            [:ul.tabs__tabs
             (doall
              (for [[tab-id tab-label _] tabs]
                [:li.tabs__tab
                 {:key tab-id
                  :class (if (= @active-tab tab-id) "tabs__tab--active" "")
                  :on-click #(reset! active-tab tab-id)}
                 tab-label
                 ]))]
            (first (keep (fn [[id _ val]]
                           (when (= id @active-tab) val))
                         tabs))])
         (first tabs)))
          
     [:div.align-right {:style {:margin-top "2em"}}
      [:button.button.button--danger
       {:on-click popover/close!
        :style {:margin-left :auto}}
       "Cancel"]
      [:button.button
       {:on-click (fn []
                    (state/edit! document
                                 #(-> %
                                      (apply-path-state       @path-state       (map ::candidate/id paths))
                                      (apply-demand-state     @demand-state     (map ::candidate/id buildings))
                                      (apply-tariff-state     @tariff-state     (map ::candidate/id buildings))
                                      (apply-technology-state @technology-state (map ::candidate/id buildings))
                                      (apply-category-state @category-state
                                                            (map ::candidate/id candidates))
                                      
                                      
                                      
                                      ))
                    (popover/close!))}
       "OK"]]]
    ))



(defn show-editor! [document candidate-ids]
  (when (seq candidate-ids)
    (popover/open! [candidate-editor document candidate-ids]
                   :middle)))

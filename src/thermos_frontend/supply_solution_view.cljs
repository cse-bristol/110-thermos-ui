(ns thermos-frontend.supply-solution-view
  (:require [reagent.core :as reagent]
            [thermos-frontend.debug-box :refer [pprint-pre]]
            [thermos-specs.solution :as solution]
            [thermos-frontend.chart.core :as chart]
            [thermos-frontend.chart.colors :as colors]
            [thermos-frontend.format :as format]
            [thermos-frontend.inputs :as inputs]
            [thermos-util :refer [assoc-by]]
            [thermos-specs.view :as view]
            [thermos-specs.candidate :as candidate]))

(defn- gen-x [ys]
  (mapv vector (range 0 1 (/ (dec (count ys)))) ys))

(defn margin [left bottom right top]
  (fn [{:keys [x y width height]}]
    {:x (+ left x) :y (+ bottom y)
     :width (- width right left) :height (- height top bottom)}))

(def cost-label
  {:total-cost [:span "¤" [:sub "TOTAL"]]
   :lifetime-cost [:span "¤" [:sub "LIFE"]]
   :annual-cost "¤/yr"
   :present-cost [:span "¤" [:sub "PV"]]})

(defn- plant-name [problem plant-type]
  (-> problem :plant-options (get plant-type) :name))

(defn- store-name [problem store-type]
  (-> problem :storage-options (get store-type) :name))

(defn- summary-details
  "This is like using normal details/summary, but we don't render details
  at all if the display is closed."
  ([summary detail]
   (reagent/with-let [open (reagent/atom false)]
     [summary-details {:open open} summary detail]))
  ([{open :open} summary detail]
   [:details {:open @open}
    [:summary.card-header {:style {:margin-bottom 0 :cursor :pointer
                                   :user-select :none}
                           :on-click #(swap! open not)} summary]
    (when @open
      [:div {:style {:margin-top :1em}} detail])]))

(defn- color-square [color]
  [:span {:style {:width :0.8em :height :0.8em
                  :margin-right :0.2em
                  :margin-left :0.2em
                  :background color
                  :display :inline-block}}])

(defn- highlight-style [key *highlight atts
                        & {:keys [hi lo]
                           :or {hi {} lo {:opacity 0.25}}}]
  (let [highlight @*highlight]
    (-> atts
        (assoc
         :on-mouse-enter #(reset! *highlight key)
         :on-mouse-leave #(reset! *highlight nil))
        (cond->
            highlight
          (update :style
                  merge
                  (if (= key highlight) hi lo))))
    ))

(defn- total-cost-summary [problem solution *highlight]
  (let [total-lifetime-cost (atom nil)
        total-lifetime-heat (atom nil)]
    [:div
     [:div.flex-cols {:style {:flex-wrap :wrap}}
      [:table.supply-solution-table {:style {:margin :1em :flex-grow 1}}
       [:caption "Supply technologies"]
       [:thead
        [:tr
         [:th "Item"]
         [:th.num "Capex"]
         [:th.num "Opex"]
         [:th.num "Fuel"]
         [:th.num "Export"]
         [:th.num "Emissions"]
         [:th.num "Total"]
         [:th.num "PC"]
         ]
        [:tr {:style {:font-size :0.8em}}
         [:th]
         [:th.num (cost-label :total-cost)]
         [:th.num (cost-label :total-cost)]
         [:th.num (cost-label :total-cost)]
         [:th.num (cost-label :total-cost)]
         [:th.num (cost-label :total-cost)]
         [:th.num (cost-label :total-cost)]
         [:th.num (cost-label :present-cost)]]]
       
       (let [items
             (concat
              (for [[plant-id {:keys [capital-cost operating-cost fuel-cost
                                      grid-revenue emissions]}]
                    (:plant solution)]
                [[:plant plant-id]
                 (plant-name problem plant-id)
                 capital-cost operating-cost fuel-cost
                 grid-revenue (apply merge-with + (vals emissions))])
              (for [[store-id {:keys [capital-cost]}] (:storage solution)]
                [[:store store-id]
                 (-> (:storage-options problem)
                     (get store-id)
                     (:name))
                 capital-cost]))]
         [:tbody
          (doall
           (for [[id name cap op fuel grid ems] items]
             [:tr (highlight-style id *highlight {:key id}
                                   :hi {:background :#eee} :lo {})
              [:th name]
              [:td.num (and cap  (not (zero? cap))  (format/si-number (:total-cost cap)))]
              [:td.num (and op   (not (zero? op))   (format/si-number (:total-cost op)))]
              [:td.num (and fuel (not (zero? fuel)) (format/si-number (:total-cost fuel)))]
              [:td.num (and grid (not (zero? grid)) (format/si-number (:total-cost grid)))]
              [:td.num (and ems  (not (zero? ems))  (format/si-number (:total-cost ems)))]
              [:td.num (format/si-number
                        (+ (:total-cost cap)
                           (:total-cost op)
                           (:total-cost fuel)
                           (:total-cost grid)
                           (:total-cost ems)))
               ]
              [:td.num (format/si-number
                        (+ (:present-cost cap)
                           (:present-cost op)
                           (:present-cost fuel)
                           (:present-cost grid)
                           (:present-cost ems)))]]))

          (let [totals      (map
                             (fn [i] (reduce + (map #(:total-cost (get % i)) items)))
                             (range 2 7))
                total-total (reduce + totals)
                total-pv    (reduce + (keep :present-cost (apply concat items)))
                ]
            (reset! total-lifetime-cost total-total)
            [:tr {:style {:border-top "1px black solid"}}
             [:th "Total"]
             (into [:<> ] (for [t totals] [:td.num (format/si-number t)]))
             [:td.num (format/si-number total-total)]
             [:td.num (format/si-number total-pv)]
             ])])]

      (let [total-frequency (-> problem :day-types vals
                                (->> (map :frequency) (reduce +)))
            
            span (:accounting-period problem)

            annual-kwh
            (fn [output]
              (reduce
               +
               (for [[day-type values] output]
                 (let [{ds :divisions frq :frequency} (get (:day-types problem) day-type)
                       cell-weight (* (/ ds 24.0) (/ frq total-frequency))]
                   (* 365.0 cell-weight (reduce + values))))))]
        
        [:table.supply-solution-table {:style {:flex-grow 1}}
         [:caption "Heat production for all days"]
         [:thead [:tr [:th "Source"] [:th.num "Wh/yr"] [:th.num "Wh total"]]]
         (let [plant-rows
               (for [[plant-type {:keys [output]}] (:plant solution)]
                 [(plant-name problem plant-type) (annual-kwh output)])

               store-rows
               (for [[store-type {:keys [output]}] (:storage solution)]
                 [(store-name problem store-type) (annual-kwh output)])
               
               curt-rows
               [["Curtailment"
                 (annual-kwh (:curtailment solution))]]

               all-rows (concat plant-rows store-rows curt-rows)
               ]
           [:tbody
            (for [[label value] all-rows :when (not (zero? value))]
              [:tr {:key label}
               [:td label]
               [:td.num (format/si-number (* 1000 value))]
               [:td.num (format/si-number (* span 1000 value))]])
            (let [total (reduce + (map second all-rows))]
              (reset! total-lifetime-heat (* span total))
              [:tr {:style {:border-top "1px black solid"}}
               [:th "Total"]
               [:td.num (format/si-number (* 1000 total))]
               [:td.num (format/si-number (* span 1000 total))]]
              )]
           )])]
     [:div {:style {:margin :1em}}
      [:h3
       "Average unit cost of production: "
       (format/si-number (/ (* 100.0 @total-lifetime-cost)
                            @total-lifetime-heat))
       "c/kWh"]]
     ]))

(defn- plant-summary [problem solution *highlight capex-format opex-format]
  [:table.supply-solution-table {:style {:margin :1em :flex-grow 1}}
   [:thead
    [:tr
     [:th     "Plant"]
     [:th.num "Peak"]
     [:th.num "Output"]
     [:th.num "Capex"]
     [:th.num "Opex"]
     [:th.num "Fuel"]
     [:th.num "Export"]
     [:th.num "Emissions"]]
    [:tr {:style {:border-bottom "1px black solid" :font-size :0.8em}}
     [:th]
     [:th.num "W"]
     [:th.num "Wh/yr"]
     [:th.num (cost-label capex-format)]
     [:th.num (cost-label opex-format)]
     [:th.num (cost-label opex-format)]
     [:th.num (cost-label opex-format)]
     [:th.num (cost-label opex-format)]]]
   
   [:tbody
    (doall
     (for [[plant-id {:keys [capacity-kw capital-cost operating-cost
                             fuel-cost   output-kwh   grid-revenue
                             emissions]}]
           (:plant solution)]

       [:tr
        (highlight-style [:plant plant-id] *highlight {:key plant-id}
                         :hi {:background :#eee} :lo {})
        [:td (plant-name problem plant-id)]
        [:td.num (format/si-number (* 1000 capacity-kw))]
        [:td.num (format/si-number (* 1000 output-kwh))]
        [:td.num (format/si-number (capex-format capital-cost))]
        [:td.num (format/si-number (opex-format operating-cost))]
        [:td.num (format/si-number (opex-format fuel-cost))]
        [:td.num (format/si-number (- (opex-format grid-revenue)))]
        [:td.num (format/si-number (reduce + (map opex-format (vals emissions))))]]
       ))]])

(def chart-margin (margin 40 30 10 10))
(def axis-margin  (margin 0 2 0 0))

(def axis-style
  {:fill :none :stroke-width 1 :stroke :black
   :shape-rendering :crisp-edges})

(def tick-style {:fill :none :stroke-width 1 :stroke :black
                 :shape-rendering :crisp-edges})

(defn- heat-production-charts [problem solution *highlight]
  (let [day-types (:day-types problem)
        
        line-style {:fill :none :stroke-width 2 :stroke :black}

        heat-demands
        (for [day-type (keys day-types)]
          {:day-type day-type
           :series   [:heat-demand]
           :values   (-> problem :profile (get day-type) :heat-demand)
           :style    {:stroke :black}
           })
        
        bar-style (fn [color highlight-value]
                    (highlight-style
                     highlight-value *highlight
                     {:fill color :stroke :none}))
        
        
        heat-productions
        (concat
         (for [[plant-type {output :output}] (:plant solution)
               day-type                      (keys day-types)]
           {:day-type  day-type
            :series    [:plant plant-type]
            :values    (get output day-type)
            :name      (plant-name problem plant-type)
            :bar-style (bar-style :red [:plant plant-type])})

         (for [[store-type {output :output}] (:storage solution)
               day-type                      (keys day-types)
               ]
           {:day-type  day-type
            :series    [:store store-type]
            :values    (get output day-type)
            :name      (store-name problem store-type)
            :bar-style (bar-style :orange [:store store-type])
            })

         (for [day-type (keys day-types)]
           {:day-type  day-type
            :series    [:curtailment]
            :name      "Curtailment"
            :values    (get (:curtailment solution) day-type)
            :bar-style (bar-style :black [:curtailment])
            })
         )

        chart-lines
        (concat heat-demands heat-productions)
        
        max-heat
        (reduce max 0 (for [line chart-lines] (reduce max 0 (:values line))))

        colors (colors/for-set (set (map :series heat-productions))
                               :k 1)
        
        heat-productions (group-by :day-type heat-productions)
        heat-demands     (assoc-by heat-demands :day-type)]
    
    
    [:div.flex-cols {:style {:flex-wrap :wrap}}
     (doall
      (for [[day lines] (group-by :day-type chart-lines)
            :let        [heat-productions (get heat-productions day)
                         heat-demand-values (:values (get heat-demands day))
                         day-kwh   (*
                                    (/ 24.0 (count heat-demand-values))
                                    (reduce + heat-demand-values))
                         
                         max-heat0 (reduce max 0 heat-demand-values)
                         digits    (Math/pow 10 (Math/floor (Math/log10 max-heat0)))
                         max-heat (* (Math/ceil (/ max-heat0 digits))
                                     digits)
                         scale-heat       (fn [s] (mapv #(/ % max-heat) s))

                         series
                         (vec
                          (for [series heat-productions
                                :when (some #(not (zero? %)) (:values series))]
                            (-> series
                                (update :values scale-heat)
                                (assoc-in [:bar-style :fill]
                                          (get colors (:series series))))))
                         ]]
        [:div {:key day :style {:min-width :400px :flex-grow 1}}
         [chart/xy-chart {:style {:height :12em}}
          [chart/clip {:with chart-margin}
           [chart/axes {:line-style axis-style
                        :range      {:y [0 (* 1000 max-heat)] :x [0 24]}
                        }]
           ;; we want a scale on the y-axis
           [chart/clip {:with axis-margin}
            [chart/stacked-bar {:series series}]
            ]]
          ]

         [:div
          [:b (:name (get day-types day))]
          " — "
          (format/si-number (* 1000 max-heat0)) "Wp"
          ", "
          (format/si-number (* 1000 day-kwh)) "Wh"
          " — "
          (doall
           (for [{:keys [name series bar-style]} series]
             [:label (highlight-style
                      series *highlight
                      {:key series})
              [color-square (-> bar-style :fill)]
              name]))]
         ]))]))

(defn- heat-production-tables [problem solution *highlight]
  (let [total-frequency (-> problem :day-types vals
                            (->> (map :frequency) (reduce +)))
        
        span (:accounting-period problem)

        rows
        (into {}
              (for [[day-type _] (:day-types problem)]
                [day-type
                 (concat
                  (for [[plant-type {:keys [output]}] (:plant solution)]
                    [day-type (plant-name problem plant-type) (get output day-type)])

                  (for [[store-type {:keys [output]}] (:storage solution)]
                    [day-type (store-name problem store-type) (get output day-type)])

                  [[day-type "Curtailment" (get (:curtailment solution) day-type)]])]))

        value-cells
        (fn [output division-weight day-weight]
          (let [peak     (reduce max output)
                daily    (* division-weight (reduce + output))
                yearly   (* 365 day-weight daily)
                lifetime (* yearly span)]
            [:<>
             [:td.num (format/si-number (* 1000.0 peak))]
             [:td.num (format/si-number (* 1000.0 daily))]
             [:td.num (format/si-number (* 1000.0 yearly))]
             [:td.num (format/si-number (* 1000.0 lifetime))]])
          )
        ]
    [:div.flex-cols {:style {:flex-wrap :wrap}}
     (for [[day-type {day-name :name divisions :divisions freq :frequency}]
           (:day-types problem)]
       (let [division-weight (/ 24.0 divisions)
             day-weight      (/ freq total-frequency)]
         [:table.supply-solution-table {:style {:flex-grow 1} :key day-type}
          [:caption day-name]
          [:thead
           [:tr
            [:th "Source"] [:th.num "Wp"]
            [:th.num "Wh/day"] [:th.num "Wh/yr"] [:th.num "Wh total"]]]
          [:tbody
           (for [[_ row-title output] (get rows day-type)
                 :when (some (comp not zero?) output)]
             [:tr {:key row-title}
              [:td row-title]
              (value-cells output division-weight day-weight)])

           (let [output (apply map + (map last (get rows day-type)))]
             [:tr {:style {:border-top "1px black solid"
                           :border-bottom "1px black solid"}}
              [:th "Total"]
              (value-cells output division-weight day-weight)])]]))]
    ))

(defn store-summary [problem solution *highlight capex-format]
  [:table.supply-solution-table {:style {:margin :1em :flex-grow 1}}
   [:thead
    [:tr
     [:th "Store type"]
     [:th.num "Store size"]
     [:th.num "Peak flow"]
     [:th.num "Capital cost"]]
    [:tr {:style {:font-size :0.8em}}
     [:th]
     [:th.num "Wh"]
     [:th.num "Wp"]
     [:th.num (cost-label capex-format)]]]
   
   [:tbody
    (let [current-highlight @*highlight]
      (for [[store-id {:keys [capacity-kw capacity-kwh capital-cost]}]
            (:storage solution)]

        [:tr {:key            store-id
              :on-mouse-enter #(reset! *highlight [:store store-id])
              :on-mouse-leave #(reset! *highlight nil)
              :style
              (when (= current-highlight [:store store-id]) {:background "#eee"})}
         
         [:td (-> (:storage-options problem)
                  (get store-id)
                  (:name))]
         [:td.num (format/si-number (* 1000 capacity-kwh)) "Wh"]
         [:td.num (format/si-number (* 1000 capacity-kw)) "Wp"]
         [:td.num (format/si-number (capex-format capital-cost)) "¤"]]))]])

(defn valid-solution [doc]
  (reagent/with-let [highlight (reagent/atom nil)
                     *capex-mode (reagent/atom :total-cost)
                     *opex-mode  (reagent/atom :total-cost)]
    (let [problem  (::solution/supply-problem @doc)
          solution (::solution/supply-solution @doc)
          capex-mode @*capex-mode
          opex-mode  @*opex-mode]
      
      [:div.solution-component
       [:div.card
        
        [summary-details {:open (reagent/cursor doc [::view/view-state :supply-solution
                                                     :total-open])}
         "Total cost summary"
         [total-cost-summary problem solution highlight]]
        
        [summary-details {:open (reagent/cursor doc [::view/view-state :supply-solution
                                                     :plant-storage-open])}
         "Plant and storage"
         [:div
          [:section.display-control-section
           [:h3 "Display options"]
           [:div.flex-cols
            [:div.flex-col
             [:h4 "Capital costs:"]
             [inputs/radio-group
              {:options   [{:label
                            [:span.has-tt
                             {:title "The total cost over the whole optimisation"}
                             "Total"] :key :total-cost}
                           {:label
                            [:span.has-tt
                             {:title "The cost per device lifetime"}
                             "Lifetime"] :key :lifetime-cost}
                           {:label "Present cost" :key :present-cost}]
               :value     capex-mode
               :on-change #(do (println %)
                               (reset! *capex-mode %))}]]
            [:div.flex-col {:style {:flex-grow 1}}
             [:h4 "Other costs:"]
             [inputs/radio-group
              {:options   [{:label "Total" :key :total-cost}
                           {:label "Annual" :key :annual-cost}
                           {:label "Present cost" :key :present-cost}]
               :value     opex-mode
               :on-change #(do (println %)
                               (reset! *opex-mode %))}]]]]
          [:div.flex-cols {:style {:flex-wrap :wrap}}
           [plant-summary problem solution highlight capex-mode opex-mode]
           [store-summary problem solution highlight capex-mode]]]]
        
        [summary-details {:open (reagent/cursor doc [::view/view-state :supply-solution
                                                     :heat-production-open])}
         "Heat production"
         [:div
          [heat-production-charts problem solution highlight]
          [heat-production-tables problem solution highlight]]
         ]

        [summary-details {:open (reagent/cursor doc [::view/view-state :supply-solution
                                                     :fuel-consumption-open])}
         "Fuel consumption and grid export"

         (let [fuel-colors
               (colors/for-set (conj (set (map :fuel (map (:plant-options problem)
                                                          (keys (:plant solution)))))
                                     :grid-export))
               ]
           [:div
            (for [day-type (keys (:day-types problem))]
              [:div {:key day-type}
               (let [plant-fuel
                     (fn [plant] (-> problem :plant-options (get plant) :fuel))
                     
                     fuel-consumption
                     (reduce
                      (fn [acc [plant-id plant]]
                        (let [fuel  (plant-fuel plant-id)
                              input (get (:input plant) day-type)
                              export (get (:generation plant) day-type)
                              ]
                          (cond->
                              (update acc fuel
                                      (fn [x]
                                        (if x
                                          (mapv + x input)
                                          input)))
                            (seq export)
                            (update :grid-export
                                    (fn [x]
                                      (if x
                                        (mapv + x input)
                                        export))))))
                      {}
                      (:plant solution))
                     
                     max-consumption
                     (reduce
                      (fn [a xs] (max a (apply max xs)))
                      0 (vals fuel-consumption))
                     ]
                 [:div {:key day-type :style {:min-width :400px :flex-grow 1}}
                  [chart/xy-chart {:style {:height :12em}}
                   [chart/clip {:with chart-margin}
                    [chart/axes {:line-style axis-style
                                 :range      {:y [0 (* 1000 max-consumption)]
                                              :x [0 24]}
                                 }]
                    ;; we want a scale on the y-axis
                    [chart/clip {:with axis-margin}
                     [chart/staggered-bar
                      {:series (for [[fuel-id vals] fuel-consumption]
                                 {:values (mapv #(/ % max-consumption) vals)
                                  :bar-style
                                  {:fill (get fuel-colors fuel-id)}
                                  })}]
                     
                     ]]]
                  [:div
                   [:b (:name (get (:day-types problem) day-type))]
                   " — "
                   (for [[fuel color] fuel-colors]
                     [:label {:key fuel :style {:margin-right :0.5em}}
                      [color-square color]
                      [:b
                       (if (= fuel :grid-export)
                         "Grid export"
                         (-> problem :fuels (get fuel) :name))

                       ": "]
                      (format/si-number (* 1000.0
                                           (reduce max (get fuel-consumption fuel))))
                      "Wp, "
                      (format/si-number (* 1000.0
                                           (/ (:divisions (get (:day-types problem) day-type))
                                              24.0)
                                           (reduce + (get fuel-consumption fuel))))
                      "Wh"
                      ])
                   ]
                  
                  ])])
            
            [:div "A table showing fuel consumption & grid export by day type, with total rows"]
            ])
         ]

        [summary-details {:open (reagent/cursor doc [::view/view-state :supply-solution
                                                     :emissions-open])}
         "Emissions"
         [:div "Table about emissions"]]

        [summary-details
         "Debug problem"
         [pprint-pre (::solution/supply-problem @doc)]
         ]

        [summary-details
         "Debug solution"
         [pprint-pre (::solution/supply-solution @doc)]
         ]

        ]])))

(defn supply-solution [doc]
  (case (-> @doc ::solution/supply-solution :state)
    :uncaught-error
    [:div.card
     [:h1.card-header "Oops - something went wrong in the optimisation"]
     [:details
      [:summary "View log"]
      [:pre (-> @doc ::solution/supply-solution :log)]]]
    [valid-solution doc]))


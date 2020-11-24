(ns thermos-frontend.solution-view
  (:require [thermos-specs.solution :as solution]
            [thermos-specs.demand :as demand]
            [thermos-specs.path :as path]
            [thermos-specs.supply :as supply]
            [thermos-specs.measure :as measure]
            [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [thermos-frontend.inputs :as inputs]
            [reagent.core :as reagent]
            [clojure.string :as s]
            [thermos-frontend.format :as format]
            [thermos-frontend.virtual-table :as table]
            [goog.object :as o]
            ))

(defn- message [& contents]
  [:div {:style {:margin :1em :background :white :border "1px grey solid"
                 :padding :0.5em}}
   contents])

(defn- solution-not-found []
  [message
   [:p [:b "No solution found"] " for this problem"]
   [:p "A solution may exist, but one could not be found with the resources available."]
   [:p "Reduce the problem size, or increase the maximum running time or allowable distance from the best possible answer."]])

(defn- solution-infeasible []
  [message
   [:p "The problem is " [:b "infeasible"] "."]
   [:p "This means that no solution exists for this problem."]
   [:p "Possible explanations for this are:"]
   [:ol
    [:li
     [:p "There are required demands which are not reachable from any supply location."]]

    [:li
     [:p "The total peak demand in some part of the problem exceeds the maximum supply capacity connectable to it."]]

    [:li
     [:p "The emissions limits cannot be achieved."]]]])

(defn- problem-empty []
  [message
   [:p "The problem is " [:b "empty"] "."]
   [:p "This means that you have defined a problem in which there are no demands that can be connected to a supply."]
   [:p "Possible explanations for this are:"]
   [:ol
    [:li [:p "The supply is disconnected from all the demands in the network. Disconnected segments are coloured magenta on the map."]]
    [:li [:p "You have not specified a supply, or the supply is to small to meet any of the demands."]]
    [:li [:p "There are no demands in the network."]]]
   [:p "To fix this you need to manipulate the candidates offered to the optimiser so that there is a supply point with some reachable demands, whose constraint state is not forbidden."]
   [:p "You might want to read the " [:a {:target :_blank
                                          :href "/help/quick-start.html"} "quick start guide"] " in the help for an introduction to constructing a network. "
    "Alternatively a more detailed explanation of how to use the network editor is " [:a {:target :_blank :href "/help/networks.html"} "here"] "."]])



(defn- number-cell [& {:keys [scale] :or {scale 1}}]
  #(format/si-number (* (o/get % "cellData") scale)))

(defn- unknown-state [document]
  (reagent/with-let [show-log (reagent/atom false)]
    [:div
     [:p "The optimisation has produced an unexpected outcome."]
     [:p "This is probably a bug in the application."]
     [:div
      [:div {:style {:background "white"}
             :on-click #(swap! show-log not)}
       (if @show-log "Less..." "More...")
       (when @show-log
         [:div
          (map-indexed
           (fn [i l] [:div {:key i} l])
           (s/split (::solution/log @document) #"\n"))])]]]))

(defn invalid-solution [state document]
  [:div.solution-component
   (case state
     :empty-problem [problem-empty]
     :infeasible [solution-infeasible]
     ::noSolution [solution-not-found]
     [unknown-state document])])

(defn- unit-header [& stuff]
  [:thead.solution-table-unit-header
   [:tr
    (map-indexed (fn [i [l _]] [:th {:key i :class (when (> i 0) "numeric")} l]) stuff)]

   [:tr {:style {:font-size :small}}
    (map-indexed (fn [i [_ l]] [:th {:key i :class (when (> i 0) "numeric")} l]) stuff)]])


(defn- summary-card [{opex-mode :opex-mode
                      capex-mode :capex-mode
                      opex-label :opex-label
                      capex-label :capex-label
                      solution-members :solution-members
                      paths :paths
                      buildings :buildings
                      supplies :supplies
                      alts :alternatives
                      by-alt :by-alternative
                      demands :demands
                      insulated :insulated
                      by-insulation :by-insulation}]
  [:section
   [:table.table.table--hover {:style {:max-width :900px}}
    [:thead
     [:tr
      [:th "Item"]
      [:th.numeric "Capital cost (" capex-label ")"]
      [:th.numeric "Operating cost (" opex-label ")"]
      [:th.numeric "Operating revenue (" opex-label ")"]
      [:th.numeric "NPV (¤)" ]]]

    (let [add-up
          (fn [& {:keys [capex opex revenue]}]
            (let [tcapex (when capex   (reduce + (map capex-mode capex)))
                  topex  (when opex    (reduce + (map opex-mode opex)))
                  trev   (when revenue (reduce + (map opex-mode revenue)))
                  tpv    (- (reduce + 0 (map :present revenue))
                            (+ (reduce + 0 (map :present capex))
                               (reduce + 0 (map :present opex))))]
              {:capex tcapex :opex topex :revenue trev :present tpv}))

          rows
          [["Network"
            [["Pipework" (add-up :capex (map ::solution/pipe-capex paths))]

             ["Heat supply"
              (add-up
               :capex (map ::solution/supply-capex supplies)
               :opex  (mapcat (juxt ::solution/supply-opex
                                    ::solution/heat-cost
                                    ::solution/pumping-cost)
                              supplies))]

             ["Demands"
              (add-up
               :capex   (map ::solution/connection-capex demands)
               :revenue (map ::solution/heat-revenue demands))]

             ["Emissions"
              (add-up
               :opex (mapcat (juxt (comp vals ::solution/supply-emissions)
                                   (comp vals ::solution/pumping-emissions)) supplies))
              ]
             ]]

           ["Individual systems"
            (doall
             (conj
              (vec
               (for [[alt-name alts] by-alt]
                 [alt-name
                  (add-up
                   :capex (map (comp :capex ::solution/alternative) alts)
                   :opex (mapcat (comp (juxt :heat-cost :opex) ::solution/alternative) alts))
                  ]))
              ["Emissions"
               (add-up
                :opex (mapcat (comp vals :emissions ::solution/alternative) alts))]))]
           


           ["Insulation"
            (doall (for [[ins-name ins] by-insulation]
                     [ins-name (add-up :capex ins)]))]
           ]

          totalise
          (fn [items thing]
            (let [vs (keep thing items)]
              (when (seq vs)
                (format/si-number (reduce + vs))))
            )

          ]
      [:tbody
       (for [[row-name rows] rows
             :let [row-vals (map second rows)]]
         (list
          (doall
           (for [[row-name {cap :capex op :opex rev :revenue p :present}] rows]
             [:tr {:key row-name}
              [:th row-name]
              [:td.numeric (if cap (format/si-number cap) "--")]
              [:td.numeric (if op (format/si-number op) "--")]
              [:td.numeric (if rev (format/si-number rev) "--")]
              [:td.numeric (format/si-number p)]]))
          [:tr.totals-row {:key row-name}
           [:th row-name]
           [:td.numeric (or (totalise row-vals :capex) "--")]
           [:td.numeric (or (totalise row-vals :opex) "--")]
           [:td.numeric (or (totalise row-vals :revenue) "--")]
           [:td.numeric (or (totalise row-vals :present) "--")]]))

       (let [rows (->> rows (mapcat second) (map second))]
         [:tr.grand-totals-row
          [:th "Whole system"]
          [:td.numeric (totalise rows :capex)]
          [:td.numeric (totalise rows :opex)]
          [:td.numeric "n/a"]

          [:td.numeric.has-tt
           {:title "This does not include network revenues"}
           (format/si-number
            (- (reduce + 0
                       (keep :present rows))
               (reduce + 0
                       (keep :present (map (comp ::solution/heat-revenue) demands)))))
           ]])])]])


(defn- network-card [{opex-mode :opex-mode
                      capex-mode :capex-mode
                      opex-label :opex-label
                      capex-label :capex-label
                      solution-members :solution-members
                      paths :paths
                      buildings :buildings
                      supplies :supplies
                      alts :alternatives
                      by-alt :by-alternative
                      demands :demands
                      insulated :insulated
                      by-insulation :by-insulation
                      user-field-names :user-field-names}
                     parameters]
  (reagent/with-let [active-tab (reagent/atom :pipework)]
    [:section
     (cond
       (empty? supplies)
       "No network was constructed."

       (empty? paths)
       "A network was constructed, but it contains only the supply."

       :else
       [:div
        [:ul.tabs__tabs.tabs__tabs--pills
         (doall (for [[key title] [[:pipework "Pipework"] [:demands "Demands"] [:supplies "Supplies"]]]
                  [:li.tabs__tab {:key key
                                  :class (when (= @active-tab key) "tabs__tab--active")
                                  :on-click #(reset! active-tab key)}
                   title]))]
        [:br]
        [:ul.tabs__pages
         [:li.tabs__page {:key :pipework :class (when (= @active-tab :pipework) "tabs__page--active")}
          [:table.table.table--hover {:style {:max-width :900px}}
           [unit-header
            ["Civils"]
            ["⌀" "mm"]
            ["Length" "m"]
            ["Cost" capex-label]
            ["Cost" [:span capex-label "/m"]]
            ["Losses" "Wh/yr"]
            ["Capacity" "W"]]

           [:tbody
            (let [pipework-row
                  (fn [{key :key} civ dia pipes]
                    (when (seq pipes)
                      (let [length (reduce + (map ::path/length pipes))
                            cost (reduce + (map (comp capex-mode
                                                      ::solution/pipe-capex)
                                                pipes))
                            losses (* 1000
                                      (reduce + (map ::solution/losses-kwh
                                                     pipes)))
                            capacity (* 1000
                                        (reduce max (map ::solution/capacity-kw
                                                         pipes)))]
                        [:tr {:key key :class (when (= key :all) "grand-totals-row")}
                         [:td civ]
                         [:td.numeric dia]
                         [:td.numeric (format/si-number length)]
                         [:td.numeric (format/si-number cost)]
                         [:td.numeric (format/si-number (/ cost length))]
                         [:td.numeric (format/si-number losses)]
                         [:td.numeric (format/si-number capacity)]
                         ])))
                  pipework-groups
                  (sort-by first
                           (group-by
                            (juxt ::path/civil-cost-id ::solution/diameter-mm)
                            paths))]
              (list
               (doall
                (for [[[civ sz] paths] pipework-groups]
                  [pipework-row
                   {:key [civ sz]}
                   (document/civil-cost-name @parameters civ)
                   sz
                   paths]))
               (when (> (count pipework-groups) 1)
                 (pipework-row
                  {:key :all}
                  [:b "All"] "" paths))))]]]

         (reagent/with-let [user-field (reagent/atom (first user-field-names))]
           [:li.tabs__page {:key :demands :class (when (= @active-tab :demands) "tabs__page--active")}
            [:table.table.table--hover {:style {:max-width :900px}}
             [unit-header
              [[inputs/select {:value-atom user-field
                               :values
                               (for [n user-field-names] [n n])}]]
              
              ["Count"]
              ["Capacity" "W"]
              ["Demand" "Wh/yr"]
              ["Conn. cost" capex-label]
              ["Revenue" opex-label]]

             (let [user-field @user-field]
               [:tbody
                (doall
                 (for [[class demands] (sort-by first (group-by
                                                       (fn [x]
                                                         (get (::candidate/user-fields x)
                                                              user-field))
                                                       demands)
                                                )
                       :let [class (or class "Unclassified")]]
                   [:tr {:key class}
                    [:td class]
                    [:td.numeric (count demands)]
                    [:td.numeric (format/si-number
                                  (* 1000
                                     (reduce + 0 (map
                                                  #(candidate/peak-demand % (document/mode @parameters))
                                                  demands))))]
                    [:td.numeric (format/si-number
                                  (* 1000
                                     (reduce + 0 (map ::solution/kwh demands))))]
                    [:td.numeric (format/si-number
                                  (reduce + 0 (map (comp capex-mode ::solution/connection-capex) demands)))]

                    [:td.numeric (format/si-number
                                  (reduce + 0 (map (comp opex-mode ::solution/heat-revenue) demands)))]]))])]])

         (reagent/with-let [user-field
                            (reagent/atom
                             (if (contains? (set user-field-names) "Name")
                               "Name" ;; HACK
                               (first user-field-names)))]
           [:li.tabs__page {:key :supplies :class (when (= @active-tab :supplies) "tabs__page--active")}
            [:table.table.table--hover {:style {:max-width :900px}}
             [unit-header
              [[inputs/select {:value-atom user-field
                               :values
                               (for [n user-field-names] [n n])}] ]
              ["Capacity" "Wp"]
              ["Output" "Wh/yr"]
              ["Pumping" "Wh/yr"]
              ["Capital" capex-label]
              ["Capacity" opex-label]
              ["Heat" opex-label]
              ["Pumping" opex-label]
              ["Coincidence" "%"]]

             [:tbody
              (let [user-field @user-field
                    get-user-field #(get (::candidate/user-fields %) user-field)]
                (doall
                 (for [s (sort-by get-user-field supplies)]
                   [:tr {:key (::candidate/id s)}
                    [:td (get-user-field s)]
                    [:td.numeric (format/si-number (* 1000 (::solution/capacity-kw s)))]
                    [:td.numeric (format/si-number (* 1000 (::solution/output-kwh s)))]
                    [:td.numeric (format/si-number (* 1000 (::solution/pumping-kwh s)))]
                    [:td.numeric (format/si-number (capex-mode (::solution/supply-capex s)))]
                    [:td.numeric (format/si-number (opex-mode (::solution/supply-opex s)))]
                    [:td.numeric (format/si-number (opex-mode (::solution/heat-cost s)))]
                    [:td.numeric (format/si-number (opex-mode (::solution/pumping-cost s)))]
                    [:td.numeric (* 100 (::solution/diversity s))]])))]]])
         ]])]))

(defn- alternatives-card [{opex-mode :opex-mode
                           capex-mode :capex-mode
                           opex-label :opex-label
                           capex-label :capex-label
                           solution-members :solution-members
                           paths :paths
                           buildings :buildings
                           supplies :supplies
                           alts :alternatives
                           by-alt :by-alternative
                           demands :demands
                           insulated :insulated
                           by-insulation :by-insulation}
                          parameters]
  [:section
   (if (empty? alts)
     "No individual systems have been installed"
     [:table.table.table--hover {:style {:max-width :900px}}
      [unit-header
       ["System"]
       ["Count"]
       ["Capacity" "Wp"]
       ["Output" "Wh/yr"]
       ["Capital cost" capex-label]
       ["Capacity" opex-label]
       ["Heat" opex-label]]

      [:tbody
       
       (doall
        (for [[alt-name buildings] (sort-by first by-alt)]
          [:tr {:key alt-name}
           [:td alt-name]
           [:td.numeric (count buildings)]
           [:td.numeric (format/si-number (* 1000 (reduce + #(candidate/peak-demand % (document/mode @parameters))
                                                          buildings)))]
           [:td.numeric (format/si-number (* 1000 (reduce + (map ::solution/kwh buildings))))]
           [:td.numeric (format/si-number (reduce + (map (comp
                                                          capex-mode
                                                          :capex
                                                          ::solution/alternative)
                                                         buildings)))]

           [:td.numeric (format/si-number (reduce + (map (comp
                                                          #(opex-mode (:opex %))
                                                          ::solution/alternative)
                                                         buildings)))]
           [:td.numeric (format/si-number (reduce + (map (comp
                                                          #(opex-mode (:heat-cost %))
                                                          ::solution/alternative)
                                                         buildings)))]
           ]))]])])

(defn- insulation-card [{opex-mode :opex-mode
                         capex-mode :capex-mode
                         opex-label :opex-label
                         capex-label :capex-label
                         solution-members :solution-members
                         paths :paths
                         buildings :buildings
                         supplies :supplies
                         alts :alternatives
                         by-alt :by-alternative
                         demands :demands
                         insulated :insulated
                         by-insulation :by-insulation}]
  [:section
   (if (empty? insulated)
     "No buildings were insulated"
     (let [all-insulations
           (group-by ::measure/id
                     (mapcat ::solution/insulation insulated))]
       [:table.table.table--hover {:style {:max-width :900px}}
        [unit-header
         ["Insulation"]
         ["Count"]
         ["Area" [:span "m" [:sup "2"]]]
         ["Effect" "Wh/yr"]
         ["Capital cost" capex-label]]
        [:tbody
         (doall (for [[k is] all-insulations]
                  [:tr {:key k}
                   [:td (::measure/name (first is))]
                   [:td.numeric (count is)]
                   [:td.numeric (format/si-number (reduce + 0 (map :area is)))]
                   [:td.numeric (format/si-number (* 1000 (reduce + 0 (map :kwh is))))]
                   [:td.numeric (format/si-number (reduce + 0 (map capex-mode is)))]]))]]))])


(defn- emissions-card [{opex-mode :opex-mode
                        capex-mode :capex-mode
                        opex-label :opex-label
                        capex-label :capex-label
                        solution-members :solution-members
                        paths :paths
                        buildings :buildings
                        supplies :supplies
                        alts :alternatives
                        by-alt :by-alternative
                        demands :demands
                        insulated :insulated
                        by-insulation :by-insulation}]
  [:section
   (let [sum-alt
         (fn [e alts what]
           (reduce +
                   0
                   (map (fn [a]
                          (get-in a [::solution/alternative :emissions e what] 0))
                        alts)))

         alt-emissions
         (into {}
               (for [[alt alts] by-alt]
                 [alt
                  (into {}
                        (for [e candidate/emissions-types]
                          [e
                           {:t (/ (sum-alt e alts :kg) 1000)
                            :cost (sum-alt e alts opex-mode)}]))]))

         
         total-alt-emissions
         (apply merge-with + (vals alt-emissions))

         sum-counter
         (fn [e what]
           (reduce + 0
                   (map #(get-in % [::solution/counterfactual :emissions e what] 0)
                        buildings)))

         counter-emissions
         (into {}
               (for [e candidate/emissions-types]
                 [e
                  {:t (/ (sum-counter e :kg)  1000)
                   :cost (sum-counter e opex-mode)}]))

         sum-supply
         (fn [k e what]
           (reduce + 0
                   (map #(get-in % [k e what] 0)
                        supplies)))

         supply-emissions
         (into {}
               (for [e candidate/emissions-types]
                 [e {:t (/ (sum-supply ::solution/supply-emissions e :kg) 1000)
                     :cost (sum-supply ::solution/supply-emissions e opex-mode)}]))

         pumping-emissions
         (into {}
               (for [e candidate/emissions-types]
                 [e {:t (/ (sum-supply ::solution/pumping-emissions e :kg) 1000)
                     :cost (sum-supply ::solution/pumping-emissions e opex-mode)}]))
         ]


     [:table.table.table--hover.emissions-table {:style {:max-width :900px}}
      [:thead.solution-table-unit-header
       [:tr
        [:th]
        (for [e candidate/emissions-types]
          [:th.border-right {:key e
                             :style {:text-align :center :background :#eef}
                             :col-span 2} (name e)])]
       [:tr
        [:th "Cause"]
        (for [e candidate/emissions-types]
          (list
           [:th.numeric {:key [e :t]}
            "t/yr"]
           [:th.numeric {:key [e :cost]} opex-label]))]]
      [:tbody
       [:tr
        [:td "Network (heat)"]
        (for [e candidate/emissions-types
              t [:t :cost]]
          [:td.numeric {:key [e t] :style {:width :100px}}
           (format/si-number (get-in supply-emissions [e t]))])]

       [:tr
        [:td "Network (pumping)"]
        (for [e candidate/emissions-types
              t [:t :cost]]
          [:td.numeric {:key [e t] :style {:width :100px}}
           (format/si-number (get-in pumping-emissions [e t]))])]
       
       (for [[alt alt-emissions] alt-emissions]
         [:tr {:key alt}
          [:td alt]
          (for [e candidate/emissions-types
                t [:t :cost]]
            [:td.numeric {:key [e t]}
             (format/si-number (get-in alt-emissions [e t]))])])
       
       [:tr.totals-row
        [:th "Total"]
        (for [e candidate/emissions-types
              t [:t :cost]]
          [:td.numeric {:key [e t]}
           (format/si-number (+ (get-in supply-emissions [e t])
                                (get-in total-alt-emissions [e t])))])
        ]
       [:tr.totals-row
        [:th "Counterfactual"]
        (for [e candidate/emissions-types
              t [:t :cost]]
          [:td.numeric {:key [e t]}
           (format/si-number (get-in counter-emissions [e t]))])]


       [:tr.grand-totals-row
        [:th "Net"]
        (for [e candidate/emissions-types
              t [:t :cost]]
          [:td.numeric {:key [e t]}
           (format/si-number (-
                              (+ (get-in supply-emissions [e t])
                                 (get-in total-alt-emissions [e t]))
                              (get-in counter-emissions [e t])))])]]])])


(defn- optimisation-card [parameters]
  [:section
   [:table.table.table--hover {:style {:max-width :350px}}
    [:tbody
     [:tr
      [:th.has-tt
       {:title
        "This is the optimisation's objective, after fixing parameters which can only be decided after finding a solution."}

       "Objective value:"]
      [:td.numeric {:style {:white-space :nowrap}}
       (format/si-number (::solution/objective @parameters))]
      ]
     [:tr
      [:th "Runtime:"]
      [:td.numeric (format/seconds (::solution/runtime @parameters))]]

     [:tr
      [:th.has-tt
       {:title "Fixing parameters sometimes changes the solution. This is how many solutions were evaluated because of this."}
       "Iterations:"]
      [:td.numeric (::solution/iterations @parameters)]]

     [:tr
      [:th.has-tt
       {:title "This is the range of objective value change caused by fixing parameters."}
       "Iteration range:"]
      [:td.numeric
       (format/si-number
        (Math/abs
         (- (reduce min (::solution/objectives @parameters))
            (reduce max (::solution/objectives @parameters)))))]]

     [:tr
      [:th "Gap:"]
      [:td.numeric (::solution/gap @parameters)]]

     [:tr
      [:th.has-tt {:title "These are the optimiser's bounds on the optimal objective value for the best solution found, without accounting for subsequent parameter fixing."}
       "Bounds:"]
      [:td.numeric (format/si-number
                    (first (::solution/bounds @parameters)))
       " — "
       (format/si-number
        (second (::solution/bounds @parameters)))
       ]]]]])

(defn- solution-summary* [document]
  (reagent/with-let
    [solution-members (reagent/track
                       #(->> document deref
                             ::document/candidates
                             vals
                             (filter candidate/in-solution?)))

     parameters
     (reagent/track
      #(-> document deref
           (select-keys [::document/pipe-costs
                         ::document/flow-temperature
                         ::document/return-temperature

                         ::document/pumping-overhead
                         ::document/pumping-cost-per-kwh
                         
                         ::solution/objective
                         ::solution/runtime
                         ::solution/iterations
                         ::solution/bounds
                         ::solution/objectives
                         ::solution/gap
                         ])))

     *capex-mode (reagent/atom :total)
     *opex-mode  (reagent/atom :total)
     active-tab (reagent/atom :cost-summary)
     ]

    (let [capex-mode @*capex-mode
          opex-mode  @*opex-mode

          solution-members @solution-members

          user-field-names (sort (set (mapcat (comp keys ::candidate/user-fields) solution-members)))

          {paths :path
           buildings :building}
          (group-by ::candidate/type solution-members)

          alts    (filter ::solution/alternative buildings)
          by-alt  (sort-by first
                           (group-by (comp ::supply/name ::solution/alternative) alts))

          insulated
          (filter (comp seq ::solution/insulation) buildings)

          insulations
          (->> (mapcat ::solution/insulation insulated)
               (group-by ::measure/name)
               (sort-by first))

          card-arguments
          {:user-field-names user-field-names
           :opex-mode opex-mode
           :capex-mode capex-mode
           :opex-label
           (case opex-mode
             :annual "¤/yr"
             :total "¤"
             :present [:span "¤" [:sub "PV"]])
           :capex-label
           (case capex-mode
             :total "¤"
             :principal "¤₀"
             :present [:span "¤" [:sub "PV"]])
           :solution-members solution-members
           :paths paths
           :buildings buildings
           :supplies (filter candidate/supply-in-solution? buildings)
           :alternatives alts
           :by-alternative by-alt
           :demands (filter candidate/is-connected? buildings)
           :insulated insulated
           :by-insulation insulations}
          ]
      [:div.solution-component
       {:style {:flex-grow 1 :padding :1em}}

       [:div.card
        [:h2.card-header "Solution Summary"]
        [:section.display-control-section
         [:h3 "Display Options"]
         [:div.flex-cols
          [:div.flex-col
           [:h4 "Capital costs:"]
           [inputs/radio-group
            {:options [{:label "Total" :key :total}
                       {:label "Principal" :key :principal}
                       {:label "Present value" :key :present}]
             :value capex-mode
             :on-change #(reset! *capex-mode %)}]]
          [:div.flex-col {:style {:flex-grow 1}}
           [:h4 "Other costs:"]
           [inputs/radio-group
            {:options [{:label "Total" :key :total}
                       {:label "Annual" :key :annual}
                       {:label "Present value" :key :present}]
             :value opex-mode
             :on-change #(reset! *opex-mode %)}]]]]

        [:ul.tabs__tabs
         (doall
           (for [[key name]
                 [[:cost-summary "Cost summary"] [:network "Network"] [:alternatives "Individual systems"]
                  [:insulation "Insulation"] [:emissions "Emissions"] [:optimisation "Optimisation"]]]

             [:li.tabs__tab
              {:key key
               :class (when (= @active-tab key) "tabs__tab--active")
               :on-click (fn []
                           (println key)
                           (reset! active-tab key))}
              name]))]
        [:ul.tabs__pages
         [:li.tabs__page {:class (when (= @active-tab :cost-summary) "tabs__page--active")}
          [summary-card card-arguments]]
         [:li.tabs__page {:class (when (= @active-tab :network) "tabs__page--active")}
          [network-card card-arguments parameters]]
         [:li.tabs__page {:class (when (= @active-tab :alternatives) "tabs__page--active")}
          [alternatives-card card-arguments parameters]]
         [:li.tabs__page {:class (when (= @active-tab :insulation) "tabs__page--active")}
          [insulation-card card-arguments]]
         [:li.tabs__page {:class (when (= @active-tab :emissions) "tabs__page--active")}
          [emissions-card card-arguments]]
         [:li.tabs__page {:class (when (= @active-tab :optimisation) "tabs__page--active")}
          [optimisation-card parameters]]]
        ]])
    ))

(defn solution-summary [document]
  (let [state (keyword (::solution/state @document))]
    (if (solution/valid-state? state)
      [solution-summary* document]
      [invalid-solution state document])))

(defn run-log [document]
  [:pre {:style {:text-wrap :pre-wrap}}
   (::solution/log @document)
   ]
  ;; [:div {:style {:font-family "Monospace"}}
  ;;  (map-indexed
  ;;   (fn [i l] [:pre {:style {:margin 0 :padding 0}
  ;;                    :key i} l])
  ;;   (s/split (::solution/log @document) #"\n"))]
  )

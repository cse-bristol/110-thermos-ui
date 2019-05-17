(ns thermos-frontend.solution-view
  (:require [thermos-specs.solution :as solution]
            [thermos-specs.demand :as demand]
            [thermos-specs.path :as path]
            [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [reagent.core :as reagent]
            [clojure.string :as s]
            [thermos-frontend.format :as format]
            [thermos-frontend.virtual-table :as table]
            [goog.object :as o]
            ))

(defn- solution-not-found []
  [:div
   [:p [:b "No solution found"] " for this problem"]
   [:p "A solution may exist, but one could not be found with the resources available."]
   [:p "Reduce the problem size, or increase the maximum running time or allowable distance from the best possible answer."]])

(defn- solution-infeasible []
  [:div
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

(defn- solution-summary [solution-members finance-parameters objective-value runtime]
  [:div {:style {:flex-grow 1 :margin-top :1em}}
   (let [{loan-term ::document/loan-term
          loan-rate ::document/loan-rate
          npv-term ::document/npv-term
          npv-rate ::document/npv-rate} @finance-parameters
         
         annualize
         (if (zero? loan-rate)
           (fn [principal] (repeat loan-term
                                   (/ principal loan-term)))
           (fn [principal]
             (let [repayment (/ (* principal loan-rate)
                                (- 1 (/ 1 (Math/pow (+ 1 loan-rate)
                                                    loan-term))))]
               (repeat loan-term repayment))))

         pv
         (if (zero? npv-rate)
           (fn [vals] (reduce + vals))
           (fn [vals] (reduce + (map-indexed (fn [i v] (/ v (Math/pow (+ 1 npv-rate) i))) vals))))
         
         opex-pv (fn [annual] (pv (repeat npv-term annual)))

         capex-pv (fn [principal] (pv (annualize principal)))

         solution-members @solution-members
         
         {paths :path buildings :building}
         (group-by ::candidate/type solution-members)

         ;; capital cost summation

         number-of-supplies (count (filter ::solution/capacity-kw buildings))
         number-of-demands (count (filter ::solution/heat-revenue buildings))

         total-demand (reduce + (map #(if (::solution/heat-revenue %)
                                        (::demand/kwh % 0)
                                        0) buildings))

         total-peak (reduce + (map #(if (::solution/heat-revenue %)
                                      (::demand/kwp % 0)
                                      0) buildings))

         total-length (reduce + (map ::path/length paths))

         total-path-capacity (reduce + (map #(* (::path/length %) (::solution/capacity-kw %)) paths))
         total-losses (reduce + (map ::solution/losses-kwh paths))

         total-supply-capacity (reduce + (map #(::solution/capacity-kw % 0) buildings))
         total-supply-output (reduce + (map #(::solution/output-kwh % 0) buildings))
         
         network-principal (reduce + (map #(::solution/principal % 0) paths))
         network-finance   (- (reduce + (annualize network-principal)) network-principal)
         network-npv       (capex-pv network-principal)

         supply-principal (reduce + (map #(::solution/principal % 0) buildings))
         supply-finance   (- (reduce + (annualize supply-principal)) supply-principal)
         supply-npv       (capex-pv supply-principal)

         connection-cost-principal (reduce + (map #(::solution/connection-cost % 0) buildings))
         connection-cost-finance   (- (reduce + (annualize connection-cost-principal)) supply-principal)
         connection-cost-npv       (capex-pv connection-cost-principal)

         capacity-cost-annual (reduce + (map #(::solution/opex % 0) buildings))
         capacity-cost-total  (* npv-term capacity-cost-annual)
         capacity-cost-npv    (opex-pv capacity-cost-annual)

         heat-cost-annual (reduce + (map #(::solution/heat-cost % 0) buildings))
         heat-cost-total  (* npv-term heat-cost-annual)
         heat-cost-npv    (opex-pv heat-cost-annual)

         emissions-annual-cost (into {}
                                     (for [e candidate/emissions-types]
                                       [e (reduce +
                                                  (map
                                                   #(- (-> (::solution/emissions %)
                                                           (get e)
                                                           (:cost 0))
                                                       (-> (::solution/avoided-emissions %)
                                                           (get e)
                                                           (:cost 0)))
                                                   buildings))]))
         
         emissions-total-cost (into {} (for [[e a] emissions-annual-cost]
                                         [e (* a npv-term)]))
         
         emissions-npv (into {} (for [[e a] emissions-annual-cost]
                                  [e (opex-pv a)]))

         heat-revenue-annual (reduce + (map #(::solution/heat-revenue % 0) buildings))
         heat-revenue-total  (* npv-term heat-revenue-annual)
         heat-revenue-npv    (opex-pv heat-revenue-annual)


         costs-total (+ heat-cost-total
                        capacity-cost-total
                        network-principal
                        network-finance
                        supply-principal
                        supply-finance
                        connection-cost-principal
                        connection-cost-finance
                        (reduce + (map second emissions-total-cost)))
         

         costs-npv-total (+ supply-npv network-npv
                            heat-cost-npv
                            capacity-cost-npv
                            connection-cost-npv
                            (reduce + (map second emissions-npv)))
         
         rev  (fn [a]
                (if (zero? a)
                  [:td]
                  [(if (pos? a) :td :td.cost) (format/si-number (Math/abs a))]))
         cost (fn [a] (rev (- a)))
         ]
     [:div {:style {:display :flex :flex-direction :row :flex-wrap :wrap}}
      [:div {:style {:flex 1 :margin-left :1em}}
       [:h1 "Financial model"]
       [:table
        [:thead
         [:tr
          [:th "Capital costs"]
          [:th.has-tt {:title "The capital cost without any financing"} "Principal"]
          [:th.has-tt {:title "The additional cost (without any discounting) of financing a loan for the capital"} "Finance"]
          [:th.has-tt {:title "The NPV of the loan repayments for the capital cost"} "NPV"]]
         ]
        [:tbody
         [:tr [:th.has-tt {:title "Costs incurred for buying pipework."}
               "Network"]
          (cost network-principal)
          (cost network-finance)
          (cost network-npv)]
         
         [:tr [:th.has-tt {:title "Capital costs incurred for connecting to supply locations."} "Supply"]
          (cost supply-principal)
          (cost supply-finance)
          (cost supply-npv)]

         [:tr [:th.has-tt {:title "Capital costs incurred by connecting demands to the network."} "Connection"]
          (cost connection-cost-principal)
          (cost connection-cost-finance)
          (cost connection-cost-npv)]
         
         [:tr [:th "Total"]
          (cost (+ network-principal supply-principal connection-cost-principal))
          (cost (+ network-finance supply-finance connection-cost-finance))
          (cost (+ network-npv supply-npv connection-cost-npv))
          ]]
        
        [:thead
         [:tr [:th "Operating costs"] [:th "Annual"] [:th "Total"] [:th "NPV"] ]]

        [:tbody
         [:tr [:th.has-tt {:title "Annual costs related to supply capacity (plant size)."} "Capacity"]
          (cost capacity-cost-annual)
          (cost capacity-cost-total)
          (cost capacity-cost-npv)
          ]
         [:tr [:th.has-tt {:title "Annual cost related to the production of heat (inc. losses)."} "Heat"]
          (cost heat-cost-annual)
          (cost heat-cost-total)
          (cost heat-cost-npv)
          ]
         [:tr [:th "Total"]
          (cost (+ heat-cost-annual capacity-cost-annual))
          (cost (+ heat-cost-total capacity-cost-total))
          (cost (+ heat-cost-npv capacity-cost-npv))
          ]
         ]

        [:thead [:tr [:th "Emissions"] [:th "Annual"] [:th "Total"] [:th "NPV"]]]
        [:tbody
         (for [e candidate/emissions-types]
           [:tr {:key e}
            [:th (name e)]
            (cost (emissions-annual-cost e))
            (cost (emissions-total-cost e))
            (cost (emissions-npv e))])
         [:tr [:th "Total"]
          (cost (reduce + (map second emissions-annual-cost)))
          (cost (reduce + (map second emissions-total-cost)))
          (cost (reduce + (map second emissions-npv)))
          ]
         ]

        [:thead [:tr [:th "Revenue"] [:th "Annual"] [:th "Total"] [:th "NPV"]]]
        [:tbody
         [:tr [:th "Heat sold"]
          (rev heat-revenue-annual)
          (rev heat-revenue-total)
          (rev heat-revenue-npv)
          ]
         [:tr {:style {:text-decoration :underline}}
          [:th {:title (str "Objective: " (format/si-number @objective-value))} "Net of costs"]
          [:td]
          (rev (- heat-revenue-total costs-total))
          (rev (- heat-revenue-npv costs-npv-total))
          ]
         ]]]
      [:div {:style {:flex 1 :margin-left :1em}}
       [:h1 "Key quantities"]
       [:table
        [:tbody
         [:tr [:th "Model runtime"] [:td (format/seconds @runtime)]]
         [:tr [:th "Objective value"] [:td (format/si-number @objective-value)]]
         [:tr [:th "Number of supplies"] [:td number-of-supplies]]
         [:tr [:th "Supply capacity"] [:td (format/si-number (* 1000 total-supply-capacity)) "Wp"]]
         [:tr [:th "Supply output"] [:td (format/si-number (* 1000 total-supply-output)) "Wh/yr"]]
         [:tr [:th "Number of demands"] [:td number-of-demands]]
         [:tr [:th "Total demand"] [:td (format/si-number (* 1000 total-demand)) "Wh/yr"]]
         [:tr
          [:th.has-tt
           {:title (str "This is the total un-diversified demand. "
                        "Accounting for diversity gives the difference bewteen this and the supply capacity.")}
           "Total peak"] [:td (format/si-number (* 1000 total-peak)) "Wp"]]
         [:tr [:th "Length of network"] [:td (format/si-number total-length) "m"]]
         [:tr [:th "Heat losses"] [:td (format/si-number (* 1000 total-losses)) "Wh/yr ("(format/si-number (/ (* 100 total-losses) total-supply-output)) "%)"]]
         [:tr [:th "Capacity of network"] [:td (format/si-number (* 1000 total-path-capacity)) "Wm"]]]]
       [:h2 "Emissions"]
       [:table
        [:thead
         [:tr [:th]
          (for [e candidate/emissions-types]
            [:th {:key e} (name e) " (g/yr)"])]]
        (let [emissions
              (for [e candidate/emissions-types]
                [e
                 (* 1000 (reduce + (map #(-> % ::solution/emissions (get e) (:kg 0)) buildings)))
                 (* 1000 (reduce + (map #(-> % ::solution/avoided-emissions (get e) (:kg 0)) buildings)))])]
          [:tbody
           [:tr [:th "Created"]
            (for [[e c _] emissions]
              [:td {:key e} (format/si-number c)])]
           [:tr [:th "Avoided"]
            (for [[e _ a] emissions]
              [:td {:key e} (format/si-number a)])]
           
           [:tr [:th "Net"]
            (for [[e c a] emissions]
              [:td {:key e} (format/si-number (- c a))])]])]]])])

(defn- number-cell [& {:keys [scale] :or {scale 1}}]
  #(format/si-number (* (o/get % "cellData") scale)))

(defn- demands-list [solution-members]
  (let [demands (filter candidate/has-demand? @solution-members)]
    [:div {:style {:flex-grow 1}}
     [table/component
      {:items demands}
      {:width 200 :flexGrow 1 :label "Name"                :key ::candidate/name}
      {:width 200 :flexGrow 1 :label "Classification"      :key ::candidate/subtype}
      {:width 200 :label "Demand (Wh/yr)"      :key ::demand/kwh             :cellRenderer (number-cell :scale 1000)}
      {:width 200 :label "Connection size (W)" :key ::demand/kwp             :cellRenderer (number-cell :scale 1000)}
      {:width 200 :label "Heat price (c/kWh)"  :key ::demand/price           :cellRenderer (number-cell :scale 100)
       :cellDataGetter #(let [c (o/get % "rowData" nil)]
                          (/ (::solution/heat-revenue c)
                             (::demand/kwh c)))}
      {:width 200 :label "Revenue (¤/yr)"      :key ::solution/heat-revenue  :cellRenderer (number-cell)}
      ]
     ]))


(defn- network-list [solution-members]
  (let [paths (filter candidate/is-path? @solution-members)]
    [:div {:style {:flex-grow 1}}
     [table/component
      {:items paths}
      {:width 200 :flexGrow 1 :label "Name"                :key ::candidate/name}
      {:width 200 :flexGrow 1 :label "Classification"      :key ::candidate/subtype}
      {:width 200 :label "Length (m)"    :key ::path/length        :cellRenderer (number-cell)}
      {:width 200 :label "Principal (¤)" :key ::solution/principal :cellRenderer (number-cell)}
      {:width 200 :label "Capacity (W)" :key ::solution/capacity-kw :cellRenderer (number-cell :scale 1000)}
      {:width 200 :label "Diversity" :key ::solution/diversity :cellRenderer (number-cell)}
      ]]))

(defn- supply-list [solution-members]
  [:div {:style {:flex-grow 1 :overflow-y :auto}}
   (let [supplies (filter #(and (::solution/capacity-kw %)
                                (candidate/is-building? %)) @solution-members)]
     (for [s supplies]
       [:div.supply-card {:key (::candidate/id s)}
        [:h1 (or (::candidate/name s) "Unnamed building")]
        [:table
         [:tbody
          [:tr [:th "Capacity"]
           [:td (format/si-number (::solution/capacity-kw s) "kWp")]]
          [:tr [:th "Output"]
           [:td (format/si-number (::solution/output-kwh s) "kWh/yr")]]
          
          [:tr [:th "Diversity factor"] [:td (::solution/diversity s)]]
          [:tr [:th "Principal"]
           [:td (format/si-number (::solution/principal s)) "¤"]]]]]))
   
   ])

(defn- solution-exists [document]
  (reagent/with-let [candidates (reagent/cursor document [::document/candidates])
                     finance-parameters (reagent/cursor document [::solution/finance-parameters])
                     solution-members (reagent/track #(filter candidate/in-solution? (vals @candidates)))
                     solution-state (reagent/cursor document [::solution/state])
                     runtime (reagent/cursor document [::solution/runtime])
                     objective-value (reagent/cursor document [::solution/objective])

                     active-tab (reagent/atom :summary)
                     tab (fn [key label]
                           [(if (= @active-tab key) :button.selected :button)
                            {:key key
                             :on-click #(reset! active-tab key)}
                            label])
                     tabs (fn [stuff]
                            [:div {:style {:display :flex :flex-grow 1
                                           :flex-direction :column}}
                             [:div.tabs
                              (doall (for [[k v] stuff]
                                       (tab k (s/capitalize (name k)))))]
                             (stuff @active-tab)])]
    [tabs
     {:summary [solution-summary solution-members finance-parameters objective-value runtime]
      :demands [demands-list solution-members]
      :supply  [supply-list solution-members]
      :network [network-list solution-members]
      }]))


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
           (fn [i l]
             [:div {:key i} l]
             )
           (s/split (::solution/log @document) #"\n"))])]]]))

(defn component [document]
  [:div.solution-component
   (case (keyword (::solution/state @document))
     :infeasible [solution-infeasible]
     ::noSolution [solution-not-found]
     (:valid :feasible :optimal :globallyOptimal :locallyOptimal :maxIterations :maxTimeLimit)
     [solution-exists document]

     [unknown-state document])])


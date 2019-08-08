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

(defn demands-list [document]
  (reagent/with-let [demands
                     (reagent/track
                      #(filter (fn [c] (and (candidate/in-solution? c)
                                            (candidate/has-demand? c)))
                               (vals (::document/candidates @document))))]
    (let [demands @demands]
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
       ])))


(defn network-list [document]
  (reagent/with-let [paths
                     (reagent/track
                      #(filter (fn [c] (and (candidate/in-solution? c)
                                            (candidate/is-path? c)))
                               (vals (::document/candidates @document))))]
    [table/component
     {:items @paths}
     {:width 200 :flexGrow 1 :label "Name"                :key ::candidate/name}
     {:width 200 :flexGrow 1 :label "Classification"      :key ::candidate/subtype}
     {:width 200 :label "Length (m)"    :key ::path/length        :cellRenderer (number-cell)}
     {:width 200 :label "Principal (¤)" :key ::solution/principal :cellRenderer (number-cell)}
     {:width 200 :label "Capacity (W)" :key ::solution/capacity-kw :cellRenderer (number-cell :scale 1000)}
     {:width 200 :label "Diversity" :key ::solution/diversity :cellRenderer (number-cell)}
     ]))

(defn supply-list [document]
  (reagent/with-let [supplies
                     (reagent/track
                      #(filter (fn [c] (and (candidate/in-solution? c)
                                            (::solution/capacity-kw c)
                                            (candidate/is-building? c)))
                               (vals (::document/candidates @document))))]
    [:div {:style {:flex-grow 1 :overflow-y :auto}}
     (for [s @supplies]
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
           [:td (format/si-number (::solution/principal s)) "¤"]]]]])
     
     ]))

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

(defn- solution-summary* [document]
  (reagent/with-let [candidates (reagent/cursor document [::document/candidates])
                     solution-members (reagent/track #(filter candidate/in-solution? (vals @candidates)))
                     solution-state (reagent/cursor document [::solution/state])
                     runtime (reagent/cursor document [::solution/runtime])
                     objective-value (reagent/cursor document [::solution/objective])]
    [:div {:style {:flex-grow 1 :margin-top :1em}}
     (let [solution-members @solution-members
           
           {paths :path buildings :building}
           (group-by ::candidate/type solution-members)

           ;; capital cost summation

           number-of-supplies (count (filter ::solution/capacity-kw buildings))
           number-of-demands (count (filter ::solution/heat-revenue buildings))

           total-demand (reduce + (map #(if (::solution/heat-revenue %)
                                          (::solution/kwh % 0)
                                          0) buildings))

           total-insulation (reduce + (map :kwh (mapcat
                                                 ::solution/insulation
                                                 buildings)))

           total-peak (reduce + (map #(if (::solution/heat-revenue %)
                                        (::demand/kwp % 0)
                                        0) buildings))

           total-length (reduce + (map ::path/length paths))

           total-path-capacity (reduce + (map #(* (::path/length %) (::solution/capacity-kw %)) paths))
           total-losses (reduce + (map ::solution/losses-kwh paths))

           total-supply-capacity (reduce + (map #(::solution/capacity-kw % 0) buildings))
           total-supply-output (reduce + (map #(::solution/output-kwh % 0) buildings))
           
           all-costs
           (concat
            (map ::solution/pipe-capex solution-members)
            (map ::solution/supply-capex solution-members)
            (map ::solution/supply-opex solution-members)
            (map ::solution/heat-cost solution-members)
            (map ::solution/connection-capex solution-members)
            (map ::solution/heat-revenue solution-members)

            (map (comp :capex ::solution/alternative) solution-members)
            (map (comp :opex ::solution/alternative) solution-members)
            (map (comp :heat-cost ::solution/alternative) solution-members)

            (mapcat ::solution/insulation solution-members)
            (mapcat (comp vals ::solution/emissions) solution-members))

           all-costs (filter identity all-costs)
           
           all-costs
           (group-by :type all-costs)

           all-costs
           (for [[type costs] all-costs]
             (let [sum (apply merge-with +
                              (for [cost costs]
                                (dissoc cost :type)))]
               [type sum]))

           all-costs (into {} all-costs)
           
           rev  (fn [a]
                  (if (zero? a)
                    [:td]
                    [(if (pos? a) :td :td.cost) (format/si-number (Math/abs a))]))
           cost (fn [a] (rev (- a)))]
       
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
           (let [items
                 (select-keys
                  all-costs
                  [:pipe-capex :supply-capex :connection-capex
                   :insulation-capex :alternative-capex])
                 total
                 (apply merge-with + (vals items))]
             (list
              (for [[item val] items]
                [:tr {:key item}
                 [:th (name item)]
                 (cost (:principal val 0))
                 (cost (- (:total val 0)
                          (:principal val 0)))
                 (cost (:present val 0))])
              [:tr {:key :total}
               [:th "Total"]
               (cost (:principal total 0))
               (cost (- (:total total 0)
                        (:principal total 0)))
               (cost (:present total 0))]))]
          
          [:thead
           [:tr [:th "Operating costs"] [:th "Annual"] [:th "Total"] [:th "NPV"] ]]

          [:tbody
           (let [items
                 (select-keys
                  all-costs
                  [:supply-opex
                   :supply-heat
                   :alternative-opex])

                 total (apply merge-with + (vals items))]
             (list
              (for [[item val] items]
                [:tr {:key item}
                 [:th (name item)]
                 (cost (:annual val 0))
                 (cost (:total val 0))
                 (cost (:present val 0))])
              [:tr {:key :total}
               [:th "Total"]
               (cost (:annual total 0))
               (cost (:total total 0))
               (cost (:present total 0))])
             )
           
           ]
          
          [:thead [:tr [:th "Emissions"] [:th "Annual"] [:th "Total"] [:th "NPV"]]]
          [:tbody
           (let [items
                 (select-keys
                  all-costs
                  candidate/emissions-types)

                 total (apply merge-with + (vals items))]
             (list
              (for [[item val] items]
                [:tr {:key item}
                 [:th (name item)]
                 (cost (:annual val 0))
                 (cost (:total val 0))
                 (cost (:present val 0))])
              [:tr {:key :total}
               [:th "Total"]
               (cost (:annual total 0))
               (cost (:total total 0))
               (cost (:present total 0))])
             )]


          [:thead [:tr [:th "Revenue"] [:th "Annual"] [:th "Total"] [:th "NPV"]]]
          
          (let [revenue (get all-costs :heat-revenue)
                costs (apply merge-with + (vals
                                           (dissoc all-costs
                                                   :heat-revenue)))]
            [:tbody
             [:tr [:th "Heat sold"]
              (rev (revenue :annual 0))
              (rev (revenue :total 0))
              (rev (revenue :present 0))]
             
             [:tr {:style {:text-decoration :underline}}
              [:th {:title (str "Objective: " (format/si-number @objective-value))} "Net of costs"]
              [:td]
              (rev (- (revenue :total 0)
                      (costs :total 0)))
              (rev (- (revenue :present 0)
                      (costs :present 0)))
              ]
             ])]]

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
           [:tr [:th "Total demand reduction"]
            [:td (format/si-number (* 1000 total-insulation)) "Wh/yr"]]
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
                [:td {:key e} (format/si-number (- c a))])]])]]])]))

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


;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.solution-view
  (:require [thermos-frontend.doc :as doc]
            [thermos-specs.solution :as solution]
            [thermos-specs.demand :as demand]
            [thermos-specs.path :as path]
            [thermos-specs.supply :as supply]
            [thermos-specs.measure :as measure]
            [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [thermos-frontend.inputs :as inputs]
            [thermos-frontend.comparison :as comparison]
            [thermos-frontend.theme :as theme]
            [re-frame.core :as rf]
            [reagent.core :as reagent]
            [clojure.string :as s]
            [thermos-frontend.format :as format]
            [goog.object :as o]
            [thermos-util.solution-summary :as solution-summary]
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

(defn- unknown-state []
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
           (s/split (doc/value [::solution/log]) #"\n"))])]]]))

(defn invalid-solution [state]
  [:div.solution-component
   (case state
     :empty-problem [problem-empty]
     :infeasible [solution-infeasible]
     :time-limit [solution-not-found]
     [unknown-state])])

;; Tables of figures, which can compare this network with another (see
;; comparison/table-rows). A table has `columns` of figures
;; [{:label :unit :group :sum? :format :title}], after the columns which
;; say what each row is, and its rows are [{:key :labels :values :class
;; :title :format :summary?}], where a :summary? row, like a total, is
;; left out if there's only one other row.

(def ^:private compare-columns
  "The columns shown for each figure, by the way of comparing"
  {nil           [:here]
   :side-by-side [:here :there]
   :with-delta   [:here :there :delta]
   :delta-only   [:delta]})

(def ^:private compare-headings {:here "This" :there "Other" :delta "Change"})

(defn- format-value [v fmt]
  (cond
    (nil? v)    "--"
    (number? v) ((or fmt format/si-number) v)
    :else       (str v)))

(defn- format-delta [d fmt]
  (when (and (number? d) (not (zero? d)))
    (str (if (pos? d) "+" "−") ((or fmt format/si-number) (Math/abs d)))))

(defn- figures-table
  "A table of the figures in rows `here`, and `there` if `compare`, which
  is how to show them: :side-by-side, :with-delta or :delta-only"
  [{:keys [label-headers columns here there compare]}]
  (let [subcolumns (compare-columns compare)
        span       (count subcolumns)
        rows       (comparison/table-rows columns here (when compare there))
        rows       (if (<= (count (remove :summary? rows)) 1) (remove :summary? rows) rows)
        groups     (when (some :group columns)
                     (partition-by (comp :group second) (map-indexed vector columns)))
        ;; the columns after which there's a line
        starts     (cond compare (set (range (count columns)))
                         groups  (set (map ffirst groups))
                         :else   #{})
        start      (fn [i] (when (starts i) "group-start"))
        blanks     (fn [] (for [i (range (count label-headers))] [:th {:key i}]))]
    [:table.table.table--hover.figures-table
     {:style (when-not compare {:max-width :900px})}
     (some->>
      [(when groups
        [:tr
         (blanks)
         (for [g groups :let [[i {:keys [group]}] (first g)]]
           [:th {:key i :class (start i) :col-span (* span (count g))
                 :style {:text-align :center :background :#eef}}
            group])])
      (when (or (seq label-headers) (some :label columns))
        [:tr
         (for [[i h] (map-indexed vector label-headers)] [:th {:key i} h])
         (for [[i {:keys [label title]}] (map-indexed vector columns)]
           [:th.numeric {:key i :col-span span :title title
                         :class [(start i) (when title "has-tt")]
                         :style (when compare {:text-align :center})}
            label])])
      (when (some :unit columns)
        [:tr {:style {:font-size :small}}
         (blanks)
         (for [[i {:keys [unit]}] (map-indexed vector columns)]
           [:th.numeric {:key i :col-span span :class (start i)
                         :style (when compare {:text-align :center})}
            unit])])
      (when compare
        [:tr {:style {:font-size :small}}
         (blanks)
         (for [i (range (count columns))
               [j s] (map-indexed vector subcolumns)]
           [:th.numeric {:key [i j] :class (when (zero? j) (start i))}
            (compare-headings s)])])]
      (remove nil?)
      (seq)
      (into [:thead.solution-table-unit-header]))
     [:tbody
      (for [{:keys [key labels class title here there delta] row-format :format} rows]
        [:tr {:key (pr-str key) :class class}
         (for [[i l] (map-indexed vector labels)]
           [(if class :th :td) {:key i :title title :class (when title "has-tt")} l])
         (for [[i column] (map-indexed vector columns)
               :let [fmt (or row-format (:format column))]
               [j s] (map-indexed vector subcolumns)]
           [:td.numeric {:key [i j]
                         :class (when (zero? j) (start i))
                         :style (when (= :delta s) {:color theme/dark-grey})}
            (case s
              :here  (format-value (get here i) fmt)
              :there (format-value (get there i) fmt)
              :delta (format-delta (get delta i) fmt))])])]]))

(defn- solution-data
  "What the solution summary shows of document `doc`"
  [doc]
  (let [members (filter candidate/in-solution? (vals (::document/candidates doc)))
        {paths :path buildings :building} (group-by ::candidate/type members)
        alts (filter ::solution/alternative buildings)]
    {:doc              doc
     :mode             (document/mode doc)
     :user-field-names (set (mapcat (comp keys ::candidate/user-fields) members))
     :paths            paths
     :buildings        buildings
     :supplies         (filter candidate/supply-in-solution? buildings)
     :alternatives     alts
     :by-alternative   (sort-by first (group-by (comp ::supply/name ::solution/alternative) alts))
     :demands          (filter candidate/is-connected? buildings)
     :insulated        (filter (comp seq ::solution/insulation) buildings)}))

(defn- sum [f xs] (reduce + 0 (keep f xs)))

;; The cards. Each takes `here`, `there` (nil if not comparing) as from
;; solution-data, how to `compare` them, and how to show costs.

(defn- cost-summary-rows [{:keys [doc]} capex-mode opex-mode]
  (let [{:keys [rows grand-total]} (solution-summary/data-table doc capex-mode opex-mode)
        values (juxt :capex :opex :revenue :equivalized-cost :present)]
    (concat
     (for [{:keys [name subcategories total]} rows
           row (concat
                (for [{sub-name :name :as sub} subcategories]
                  {:key [name sub-name] :labels [sub-name]
                   :values (values (assoc (:value sub) :equivalized-cost (:equivalized-cost sub)))})
                [{:key name :labels [name] :values (values total) :class "totals-row"}])]
       row)
     [{:key :whole-system :labels ["Whole system"] :class "grand-totals-row"
       :title "The NPV does not include network revenues"
       :values [(:capex grand-total) (:opex grand-total) nil nil (:present grand-total)]}])))

(defn- summary-card [{:keys [here there compare capex-mode opex-mode capex-label opex-label]}]
  [:section
   [figures-table
    {:compare compare
     :label-headers ["Item"]
     :columns [{:label [:<> "Capital cost (" capex-label ")"]}
               {:label [:<> "Operating cost (" opex-label ")"]}
               {:label [:<> "Operating revenue (" opex-label ")"]}
               {:label "LCH (c/kWh)" :sum? false
                :title "Levelized cost of heat - this is the present cost (no revenues) divided by the (discounted) heat delivered."}
               {:label "NPV (¤)"}]
     :here  (cost-summary-rows here capex-mode opex-mode)
     :there (some-> there (cost-summary-rows capex-mode opex-mode))}]])

(defn- pipework-rows [{:keys [doc paths]} capex-mode]
  (let [row (fn [k labels pipes]
              (let [length (sum ::path/length pipes)
                    cost   (sum (comp capex-mode ::solution/pipe-capex) pipes)]
                {:key k :labels labels
                 :values [length
                          cost
                          (when (pos? length) (/ cost length))
                          (* 1000 (sum ::solution/losses-kwh pipes))
                          (* 1000 (reduce max 0 (keep ::solution/capacity-kw pipes)))]}))
        groups (sort-by first
                        (group-by
                         (fn [path]
                           [(cond
                              (::solution/upgraded path) "Upgraded existing pipe"
                              (::path/exists path)       "Existing pipe"
                              :else (document/civil-cost-name doc (::path/civil-cost-id path)))
                            (::solution/diameter-mm path)])
                         paths))]
    (when (seq paths)
      (concat
       (for [[[civ dia :as k] pipes] groups] (row k [civ dia] pipes))
       [(assoc (row :all [[:b "All"] ""] paths) :class "grand-totals-row" :summary? true)]))))

(defn- pipework-table [{:keys [here there compare capex-mode capex-label]}]
  [figures-table
   {:compare compare
    :label-headers ["Civils" "⌀ (mm)"]
    :columns [{:label "Length" :unit "m"}
              {:label "Cost" :unit capex-label}
              {:label "Cost" :unit [:span capex-label "/m"] :sum? false}
              {:label "Losses" :unit "Wh/yr"}
              {:label "Capacity" :unit "W" :sum? false}]
    :here  (pipework-rows here capex-mode)
    :there (some-> there (pipework-rows capex-mode))}])

(defn- user-field-names [here there]
  (sort (into (:user-field-names here) (:user-field-names there))))

(defn- demand-rows [{:keys [demands mode]} user-field capex-mode opex-mode]
  (for [[class demands] (sort-by first (group-by #(get (::candidate/user-fields %) user-field)
                                                 demands))
        :let [class (or class "Unclassified")]]
    {:key class :labels [class]
     :values [(count demands)
              (* 1000 (sum #(candidate/solved-peak-demand % mode) demands))
              (* 1000 (sum ::solution/kwh demands))
              (sum (comp capex-mode ::solution/connection-capex) demands)
              (sum (comp opex-mode ::solution/heat-revenue) demands)]}))

(defn- demands-table [{:keys [here there compare capex-mode opex-mode capex-label opex-label]}]
  (reagent/with-let [user-field (reagent/atom (first (user-field-names here there)))]
    [figures-table
     {:compare compare
      :label-headers [[inputs/select {:value-atom user-field
                                      :values (for [n (user-field-names here there)] [n n])}]]
      :columns [{:label "Count"}
                {:label "Capacity" :unit "W"}
                {:label "Demand" :unit "Wh/yr"}
                {:label "Conn. cost" :unit capex-label}
                {:label "Revenue" :unit opex-label}]
      :here  (demand-rows here @user-field capex-mode opex-mode)
      :there (some-> there (demand-rows @user-field capex-mode opex-mode))}]))

(defn- supply-rows [{:keys [supplies]} user-field capex-mode opex-mode]
  (let [user-field-of #(get (::candidate/user-fields %) user-field)]
    (for [s (sort-by user-field-of supplies)]
      {:key (::candidate/id s) :labels [(user-field-of s)]
       :values [(some-> (::solution/capacity-kw s) (* 1000))
                (some-> (::solution/output-kwh s) (* 1000))
                (some-> (::solution/pumping-kwh s) (* 1000))
                (capex-mode (::solution/supply-capex s))
                (opex-mode (::solution/supply-opex s))
                (opex-mode (::solution/heat-cost s))
                (opex-mode (::solution/pumping-cost s))
                (some-> (::solution/diversity s) (* 100))]})))

(defn- supplies-table [{:keys [here there compare capex-mode opex-mode capex-label opex-label]}]
  (reagent/with-let [user-field
                     (reagent/atom
                      (let [names (user-field-names here there)]
                        (if (contains? (set names) "Name")
                          "Name" ;; HACK
                          (first names))))]
    [figures-table
     {:compare compare
      :label-headers [[inputs/select {:value-atom user-field
                                      :values (for [n (user-field-names here there)] [n n])}]]
      :columns [{:label "Capacity" :unit "Wp"}
                {:label "Output" :unit "Wh/yr"}
                {:label "Pumping" :unit "Wh/yr"}
                {:label "Capital" :unit capex-label}
                {:label "Capacity" :unit opex-label}
                {:label "Heat" :unit opex-label}
                {:label "Pumping" :unit opex-label}
                {:label "Coincidence" :unit "%" :sum? false}]
      :here  (supply-rows here @user-field capex-mode opex-mode)
      :there (some-> there (supply-rows @user-field capex-mode opex-mode))}]))

(defn- network-card [{:keys [here there] :as arguments}]
  (reagent/with-let [active-tab (reagent/atom :pipework)]
    (let [both (remove nil? [here there])]
      [:section
       (cond
         (every? (comp empty? :supplies) both)
         "No network was constructed."

         (every? (comp empty? :paths) both)
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
            [pipework-table arguments]]
           [:li.tabs__page {:key :demands :class (when (= @active-tab :demands) "tabs__page--active")}
            [demands-table arguments]]
           [:li.tabs__page {:key :supplies :class (when (= @active-tab :supplies) "tabs__page--active")}
            [supplies-table arguments]]]])])))

(defn- alternative-rows [{:keys [by-alternative mode]} capex-mode opex-mode]
  (for [[alt-name buildings] by-alternative
        :let [alt-sum (fn [f] (sum (comp f ::solution/alternative) buildings))]]
    {:key alt-name :labels [alt-name]
     :values [(count buildings)
              (* 1000 (sum #(candidate/solved-peak-demand % mode) buildings))
              (* 1000 (sum ::solution/kwh buildings))
              (alt-sum (comp capex-mode :capex))
              (alt-sum (comp opex-mode :opex))
              (alt-sum (comp opex-mode :heat-cost))]}))

(defn- alternatives-card [{:keys [here there compare capex-mode opex-mode capex-label opex-label]}]
  [:section
   (if (every? (comp empty? :alternatives) (remove nil? [here there]))
     "No individual systems have been installed"
     [figures-table
      {:compare compare
       :label-headers ["System"]
       :columns [{:label "Count"}
                 {:label "Capacity" :unit "Wp"}
                 {:label "Output" :unit "Wh/yr"}
                 {:label "Capital cost" :unit capex-label}
                 {:label "Capacity" :unit opex-label}
                 {:label "Heat" :unit opex-label}]
       :here  (alternative-rows here capex-mode opex-mode)
       :there (some-> there (alternative-rows capex-mode opex-mode))}])])

(defn- insulation-rows [{:keys [insulated]} capex-mode]
  (for [[measure-name is] (sort-by first (group-by ::measure/name (mapcat ::solution/insulation insulated)))]
    {:key measure-name :labels [measure-name]
     :values [(count is) (sum :area is) (* 1000 (sum :kwh is)) (sum capex-mode is)]}))

(defn- insulation-card [{:keys [here there compare capex-mode capex-label]}]
  [:section
   (if (every? (comp empty? :insulated) (remove nil? [here there]))
     "No buildings were insulated"
     [figures-table
      {:compare compare
       :label-headers ["Insulation"]
       :columns [{:label "Count"}
                 {:label "Area" :unit [:span "m" [:sup "2"]]}
                 {:label "Effect" :unit "Wh/yr"}
                 {:label "Capital cost" :unit capex-label}]
       :here  (insulation-rows here capex-mode)
       :there (some-> there (insulation-rows capex-mode))}])])

(defn- emissions-rows [{:keys [supplies buildings by-alternative]} opex-mode]
  (let [;; emissions of each type from a seq of maps of them, as {e {:t :cost}}
        emissions (fn [ms]
                    (into {} (for [e candidate/emissions-types]
                               [e {:t    (/ (sum #(get-in % [e :kg]) ms) 1000)
                                   :cost (sum #(get-in % [e opex-mode]) ms)}])))
        values    (fn [em] (for [e candidate/emissions-types t [:t :cost]] (get-in em [e t])))
        plus      (fn [& ems] (apply merge-with (partial merge-with +) ems))
        heat      (emissions (map ::solution/supply-emissions supplies))
        pumping   (emissions (map ::solution/pumping-emissions supplies))
        alts      (for [[alt-name alts] by-alternative]
                    [alt-name (emissions (map (comp :emissions ::solution/alternative) alts))])
        total     (apply plus heat pumping (map second alts))
        counter   (emissions (map (comp :emissions ::solution/counterfactual) buildings))]
    (concat
     [{:key :heat :labels ["Network (heat)"] :values (values heat)}
      {:key :pumping :labels ["Network (pumping)"] :values (values pumping)}]
     (for [[alt-name em] alts] {:key [:alternative alt-name] :labels [alt-name] :values (values em)})
     [{:key :total :labels ["Total"] :class "totals-row" :values (values total)}
      {:key :counterfactual :labels ["Counterfactual"] :class "totals-row" :values (values counter)}
      {:key :net :labels ["Net"] :class "grand-totals-row"
       :values (values (plus total (update-vals counter #(update-vals % -))))}])))

(defn- emissions-card [{:keys [here there compare opex-mode opex-label]}]
  [:section
   [figures-table
    {:compare compare
     :label-headers ["Cause"]
     :columns (for [e candidate/emissions-types
                    [label unit] [["Emitted" "t/yr"] ["Cost" opex-label]]]
                {:group (name e) :label label :unit unit})
     :here  (emissions-rows here opex-mode)
     :there (some-> there (emissions-rows opex-mode))}]])

(defn- tip [title label] [:span.has-tt {:title title} label])

(defn- optimisation-rows [{:keys [doc]}]
  (let [objectives (seq (::solution/objectives doc))
        [lower upper] (::solution/bounds doc)]
    (cond->
        [{:key :objective
          :labels [(tip "This is the optimisation's objective, after fixing parameters which can only be decided after finding a solution."
                        "Objective value")]
          :values [(::solution/objective doc)]}
         {:key :runtime :labels ["Runtime"] :values [(::solution/runtime doc)] :format format/seconds}
         {:key :stopped-early
          :labels [(tip "The run was stopped before its time limit, so this is the best solution the optimiser had found by then, and a better one may exist."
                        "Stopped early")]
          :values [(if (::solution/stopped-early doc) "Yes" "No")]}]

      (seq (::solution/stages doc))
      (conj {:key :method :labels [(tip "The methods the optimiser used, in order." "Method")]
             :values [(s/join ", " (map name (::solution/stages doc)))]})

      ;; a solution that didn't finish with an exact solve has no gap,
      ;; bounds or parameter-fixing iterations
      (::solution/gap doc)
      (into
       (remove
        nil?
        [{:key :iterations
          :labels [(tip "Fixing parameters sometimes changes the solution. This is how many solutions were evaluated because of this."
                        "Iterations")]
          :values [(::solution/iterations doc)] :format str}
         (when objectives
           {:key :iteration-range
            :labels [(tip "This is the range of objective value change caused by fixing parameters."
                          "Iteration range")]
            :values [(Math/abs (- (reduce min objectives) (reduce max objectives)))]})
         {:key :gap :labels ["Gap"] :values [(::solution/gap doc)] :format str}
         {:key :bounds
          :labels [(tip "These are the optimiser's bounds on the optimal objective value for the best solution found, without accounting for subsequent parameter fixing."
                        "Bounds")]
          :values [(str (format/si-number lower) " — " (format/si-number upper))]}])))))

(defn- optimisation-card [{:keys [here there compare]}]
  [:section
   [figures-table
    {:compare compare
     :columns [{:sum? false}]
     :here  (optimisation-rows here)
     :there (some-> there optimisation-rows)}]])

(defn- solution-summary* []
  (reagent/with-let
    [here (reagent/track
           #(solution-data (assoc @(rf/subscribe [:doc/cost-factors])
                                  ::document/candidates @(rf/subscribe [:doc/candidates]))))
     ;; the network being compared with, if it has a solution
     there (reagent/track
            #(let [doc @(rf/subscribe [:compare/document])]
               (when (and doc (solution/exists? doc)) (solution-data doc))))

     *capex-mode   (reagent/atom :total)
     *opex-mode    (reagent/atom :total)
     *compare-mode (reagent/atom :with-delta)
     active-tab    (reagent/atom :cost-summary)]

    (let [capex-mode @*capex-mode
          opex-mode  @*opex-mode
          comparing  @(rf/subscribe [:compare/comparing])
          other      @there
          shown      (when (not= :none @*compare-mode) other)

          card-arguments
          {:here       @here
           :there      shown
           :compare    (when shown @*compare-mode)
           :opex-mode  opex-mode
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
             :present [:span "¤" [:sub "PV"]])}]
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
          [:div.flex-col
           [:h4 "Other costs:"]
           [inputs/radio-group
            {:options [{:label "Total" :key :total}
                       {:label "Annual" :key :annual}
                       {:label "Present value" :key :present}]
             :value opex-mode
             :on-change #(reset! *opex-mode %)}]]
          (when comparing
            [:div.flex-col
             [:h4 "Compare:"]
             (if other
               [inputs/radio-group
                {:options [{:label "None" :key :none}
                           {:label "Side by side" :key :side-by-side}
                           {:label "With change" :key :with-delta}
                           {:label "Change only" :key :delta-only}]
                 :value @*compare-mode
                 :on-change #(reset! *compare-mode %)}]
               [:span "The network being compared with has no solution."])])]]

        [:ul.tabs__tabs
         (doall
           (for [[key name]
                 [[:cost-summary "Cost summary"] [:network "Network"] [:alternatives "Individual systems"]
                  [:insulation "Insulation"] [:emissions "Emissions"] [:optimisation "Optimisation"]]]

             [:li.tabs__tab
              {:key key
               :class (when (= @active-tab key) "tabs__tab--active")
               :on-click #(reset! active-tab key)}
              name]))]
        [:ul.tabs__pages
         [:li.tabs__page {:class (when (= @active-tab :cost-summary) "tabs__page--active")}
          [summary-card card-arguments]]
         [:li.tabs__page {:class (when (= @active-tab :network) "tabs__page--active")}
          [network-card card-arguments]]
         [:li.tabs__page {:class (when (= @active-tab :alternatives) "tabs__page--active")}
          [alternatives-card card-arguments]]
         [:li.tabs__page {:class (when (= @active-tab :insulation) "tabs__page--active")}
          [insulation-card card-arguments]]
         [:li.tabs__page {:class (when (= @active-tab :emissions) "tabs__page--active")}
          [emissions-card card-arguments]]
         [:li.tabs__page {:class (when (= @active-tab :optimisation) "tabs__page--active")}
          [optimisation-card card-arguments]]]]])))


(defn solution-summary []
  (if (solution/exists? (doc/value []))
    [solution-summary*]
    [invalid-solution (doc/value [::solution/state])]))

(defn run-log []
  [:pre {:style {:text-wrap :pre-wrap}}
   (doc/value [::solution/log])
   ]
  ;; [:div {:style {:font-family "Monospace"}}
  ;;  (map-indexed
  ;;   (fn [i l] [:pre {:style {:margin 0 :padding 0}
  ;;                    :key i} l])
  ;;   (s/split (::solution/log @document) #"\n"))]
  )

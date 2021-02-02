(ns thermos-frontend.selection-info-panel
  (:require [reagent.core :as reagent]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.document :as document]
            [thermos-specs.demand :as demand]
            [thermos-specs.cooling :as cooling]
            [thermos-specs.tariff :as tariff]
            [thermos-specs.path :as path]
            [thermos-specs.solution :as solution]
            [thermos-specs.view :as view]
            [thermos-specs.supply :as supply]
            [thermos-frontend.editor-state :as state]
            [thermos-frontend.operations :as operations]
            [thermos-frontend.tag :as tag]
            [thermos-frontend.format :refer [si-number local-format]]
            [thermos-util :refer [annual-kwh->kw]]
            [thermos-frontend.format :as format]
            [thermos-frontend.inputs :as inputs]
            [thermos-frontend.flow :as flow]

            [clojure.string :as string]))

(declare component)

(defn stats-loop [values]
  (loop [vmin ##Inf
         vmax ##-Inf
         vsum 0
         vcount 0
         values values]
    (if (empty? values)
      [vmin vmax vsum vcount]
      (let [[x & values] values]
        (recur
         (min vmin x)
         (max vmax x)
         (+ vsum x)
         (inc vcount)
         values)))))

(defn- row [title body & {:keys [extra-class]}]
  [:div.selection-table__row
   [:div.selection-table__cell.selection-table__cell--first-col
    title]
   [:div.selection-table__cell.selection-table__cell--second-col
    {:class extra-class}
    body]])

(defn- number-row [selection title value &
                   {:keys [unit summary scale tooltip]
                    :or {unit "" summary :sum}}]
  (let [values (keep (if scale (fn [x] (when-let [x (value x)] (* scale x))) value)
                     selection)]
    (when-not (empty? values)
      (let [[vmin vmax vsum vcount] (stats-loop values)
            vmean (/ vsum vcount)]
        [row
         (if tooltip
           [:span.has-tt {:title tooltip} title]
           title)
         [:span
          (when (> vcount 1)
            {:class "has-tt"
             :title (str "Range: " (si-number vmin) unit " — " (si-number vmax) unit
                         (case summary
                           (:min :max :sum) (str ", Mean: " (si-number vmean) unit)
                           ""))})
          (si-number
           (case summary
             :sum vsum
             :min vmin
             :max vmax
             :mean vmean
             :count vcount))
          unit]]))))

(defonce *open-rows (reagent/atom {}))

(defn- chips-row [flow selection title value
                  & {:keys [add-classes nil-value default-closed]}]
  (reagent/with-let [*open (reagent/cursor *open-rows [title])]
    (let [by-value (dissoc (group-by (if nil-value
                                       #(or (value %) nil-value)
                                       value)
                                     selection)
                           nil)
          n-groups (count by-value)
          open     @*open
          open     (or open (and (nil? open) (not default-closed)))
          ]
      
      (when-not (zero? n-groups)
        [row
         [:span (when (> n-groups 1)
                  [:span {:style {:cursor :pointer :display :inline-block}
                          :on-click #(reset! *open (not open))
                          :class (when open "rotate-90")
                          } "▶"]) " " title]
         (if (and (> n-groups 1) (not open))
           [:div {:style {:padding :0.2em}} (str n-groups " groups")]
           (let [groups (sort-by str (keys by-value))]
             (remove
              nil?
              (for [value groups :let [candidates (get by-value value)]]
                (when (and value (not-empty candidates))
                  [tag/component
                   {:key value
                    :class (when add-classes (add-classes value))
                    
                    :count (count candidates)
                    :body value
                    :close true
                    :on-select #(flow/fire!
                                 flow
                                 [:select-ids (map ::candidate/id candidates)])
                    
                    :on-close #(flow/fire!
                                flow
                                [operations/deselect-candidates
                                 (map ::candidate/id candidates)])
                    }])))
             ))
         :extra-class "selection-table-cell--tag-container"
         ]))))

(defn linear-density-row [selection model-mode]
  (let [total-kwh (reduce + 0 (keep
                               #(or (::solution/kwh %) (candidate/annual-demand % model-mode))
                               selection))
        total-m   (when (and total-kwh
                             (pos? total-kwh))
                    (reduce + 0 (keep ::path/length selection)))]
    (when (and total-kwh total-m
               (pos? total-kwh)
               (pos? total-m))
      [row
       [:span.has-tt
        {:title
         "Linear density of the selected objects. If you want to see the linear density of a solution, select only the things in the solution."}
        "Lin. density"]
       [:span (si-number (* 1000 (/ total-kwh total-m))) "Wh/m"]])))

;; TODO these maybe too slow?

(defn- building-tariff-name [doc x]
  (when (candidate/is-building? x)
    (document/tariff-name doc (::tariff/id x))))

(defn- building-profile-name [doc x]
  (when (candidate/is-building? x)
    (document/profile-name doc (::supply/profile-id x))))

(defn- path-civil-cost-name [doc x]
  (when (candidate/is-path? x)
    (document/civil-cost-name doc (::path/civil-cost-id x))))

(defn- base-cost [determinants model-mode x]
  (case (::candidate/type x)
      :path (document/path-cost x determinants) ;; args wrong way around so no workio
      :building (tariff/connection-cost
                 (document/connection-cost-for-id determinants (::tariff/cc-id x))
                 (candidate/annual-demand x model-mode)
                 (candidate/peak-demand x model-mode))
      nil))

(defn- solution-row-classes [x]
  ["solution"
   (cond
     (or (= x "network") (= x "impossible"))
     x
     
     (= x "no") "no"
     
     (.endsWith x "(existing)")
     "no"
     
     true "individual")])

(defn- cost-row [selection]
  (reagent/with-let [*capital-mode (reagent/atom :principal)]
    (let [capital-mode @*capital-mode]
      [number-row
       selection
       
       [:span.has-tt
        {:title "This includes network supply, connection costs, insulation costs and individual system costs. Principal is the capital cost only, for a single purchase. PV capex is the discounted total capital cost, including finance and re-purchasing, which is what the optimisation uses. Summed capex is the un-discounted equivalent."}
        [inputs/select
         {:style
          {:background :none
           :border :none
           :padding 0
           :margin-top 0
           :margin-bottom 0
           :margin-right 0
           :width :auto
           :height :auto
           :border-radius 0
           :margin-left "-4px"
           :display :inline}
          :value-atom *capital-mode
          :values
          {:principal "Principal"
           :present   "PV Capex"
           :total     "Σ Capex"}
          }]
        
        ]

       #(let [p  (capital-mode (::solution/pipe-capex %))
              cc (capital-mode (::solution/connection-capex %))
              sc (capital-mode (::solution/supply-capex %))
              ac (capital-mode (::solution/alternative %))
              ics (keep capital-mode (::solution/insulation %))
              ic (when (seq ics) (reduce + 0 ics))
              ]
          (when (or p cc sc ac ic)
            (+ p cc sc ac ic)))

       :units "¤"])))

(defn- losses-row [selection]
  (let [losses (keep ::solution/losses-kwh selection)
        lengths (keep ::path/length selection)
        total-kwh (reduce + 0 losses)
        total-length (reduce + 0 lengths)
        ]
    (when (pos? total-length)
      [row
       "Losses"
       [:span
        (si-number (* 1000 total-kwh)) "Wh/yr"
        ", "
        (si-number
         (/ (* 1000 (annual-kwh->kw total-kwh)) total-length))
        "W/m"]])))


(defn- custom-keys [root]
  (let [selection @(flow/view* root operations/selected-candidates)]
    
    (->
     (reduce
      (fn [acc candidate]
        (let [uf (::candidate/user-fields candidate)]
          (reduce-kv
           (fn [acc field val]
             (let [new (not (contains? acc field))
                   cur (get acc field :none)
                   nxt (cond
                         (nil? val)               :none
                         (or (= 0 val) (= 1 val)) :bit
                         (number? val)            :number
                         (boolean? val)           :boolean
                         :else :string)
                   ]
               (cond
                 (or new (= :none cur)) (assoc! acc field nxt)
                 (= cur nxt)            acc
                 (and (= :number nxt)
                      (= :bit cur))     (assoc! acc field :number)
                 :else                  (assoc! acc field :string))))
           acc
           uf)))
      
      (transient {})
      selection)
     (persistent!)
     (->> (into (sorted-map))))))


(defn component
  "The panel in the bottom right which displays some information about the currently selected candidates."
  [root]
  (let [flow        (flow/view* root dissoc ::view/view-state) ;; stripping
                                                               ;; off
                                                               ;; view
                                                               ;; state
                                                               ;; prevents
                                                               ;; us
                                                               ;; re-rendering
                                                               ;; when
                                                               ;; nothing
                                                               ;; has
                                                               ;; changed
        selection @(flow/view* flow operations/selected-candidates)
        has-solution @(flow/view* flow document/has-solution?)
        model-mode @(flow/view* flow document/mode)
        mode-name (case model-mode :cooling "Cold" "Heat")
        cost-factors @(flow/view* flow dissoc ::document/candidates)
        custom-keys (sort-by first @(flow/view flow custom-keys))
        ]

    [:div.component--selection-info
     [:header.selection-header
      (cond (empty? selection)
            "No selection"
            (empty? (rest selection)) "One candidate selected"
            :else (str (count selection) " candidates selected"))]

     (let [chips-row  (partial chips-row root selection)
           number-row (partial number-row selection)
           base-cost  (partial base-cost cost-factors model-mode)
           ]
       [:div.selection-table
        [chips-row "Type" ::candidate/type]

        (for [[field type] custom-keys]
          [:<> {:key field}
           (case type
             :number
             [number-row field (fn [x] (-> x ::candidate/user-fields (get field)))
              :summary :sum :unit "" :default-closed true]

             :bit
             [chips-row field
              (fn [x]
                (let [s (-> x ::candidate/user-fields (get field))]
                  (case s
                    1 "yes"
                    0 "no"
                    
                    nil)))
              :nil-value "None" :default-closed true]

             :boolean
             [chips-row field
              (fn [x]
                (let [s (-> x ::candidate/user-fields (get field))]
                  (case s
                    true "yes"
                    false "no"
                    nil)))
              :nil-value "None" :default-closed true]
             
             [chips-row field (fn [x]
                                (let [s (-> x ::candidate/user-fields (get field))]
                                  (if (string/blank? s) nil s)))
              
              :nil-value "None" :default-closed true])])
        
        [chips-row "Constraint" ::candidate/inclusion
         :nil-value "Forbidden" :add-classes (fn [x] ["constraint" (name x)])]
        [chips-row "Tariff"    (partial building-tariff-name cost-factors)]
        [chips-row "Edited"    (comp {false "no" true "yes" nil "no"} ::candidate/modified)]
        [chips-row "Profile"   (partial building-profile-name cost-factors)]
        [number-row "Market rate" ::solution/market-rate
         :summary :mean :unit "c/kWh" :scale 100]
        [chips-row "Civils"    (partial path-civil-cost-name cost-factors)]

        [number-row "Length" ::path/length :unit "m"]
        [number-row "Base cost" base-cost :unit "¤"
         :tooltip (str "For buildings this is the connection cost. "
                       "For paths it is the cost of the smallest pipe.")]

        [number-row (str mode-name " demand") #(candidate/annual-demand % model-mode)
         :unit "Wh/yr" :scale 1000]
        
        [number-row (str mode-name " peak") #(candidate/peak-demand % model-mode)
         :unit "Wp" :scale 1000]

        [linear-density-row selection model-mode]

        (when has-solution
          [:<>
           [chips-row "In solution" candidate/solution-description
            :nil-value "no" :add-classes solution-row-classes]
           [number-row "Coincidence" ::solution/diversity
            :summary :mean :unit "%" :scale 100]
           [number-row "Capacity" ::solution/capacity-kw
            :summary :max :unit "W" :scale 1000]
           [number-row "Diameter" ::solution/diameter-mm
            :summary :max :unit "m" :scale 0.001]

           [cost-row selection]
           
           [number-row "Revenue" (comp :annual ::solution/heat-revenue)
            :unit "¤/yr"]
           
           [losses-row selection]
           ])])]))



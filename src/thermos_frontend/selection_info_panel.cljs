;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

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
            [thermos-frontend.tag :as tag]
            [thermos-frontend.format :refer [si-number local-format]]
            [thermos-util :refer [annual-kwh->kw]]
            [thermos-frontend.format :as format]
            [thermos-frontend.inputs :as inputs]
            [re-frame.core :as rf]

            [clojure.string :as string]))

(declare component)

(defn- stats
  "[min max sum count] of the non-nil values of (value x) for the xs"
  [value xs]
  (let [acc (reduce (fn [^js acc x]
                      (when-let [v (value x)]
                        (aset acc 0 (min (aget acc 0) v))
                        (aset acc 1 (max (aget acc 1) v))
                        (aset acc 2 (+ (aget acc 2) v))
                        (aset acc 3 (inc (aget acc 3))))
                      acc)
                    (array ##Inf ##-Inf 0 0)
                    xs)]
    [(aget acc 0) (aget acc 1) (aget acc 2) (aget acc 3)]))

(defn- group-ids
  "A map from (value x) to the ids of the xs with that value, leaving out
  those whose value is nil"
  [value xs]
  (persistent!
   (reduce (fn [groups x]
             (let [k (value x)]
               (if (nil? k)
                 groups
                 (assoc! groups k (conj (get groups k []) (::candidate/id x))))))
           (transient {})
           xs)))

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
  (let [value (if scale (fn [x] (when-let [x (value x)] (* scale x))) value)
        [vmin vmax vsum vcount] (stats value selection)]
    (when (pos? vcount)
      (let [            vmean (/ vsum vcount)]
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

(defn- chips-row [selection title value
                  & {:keys [add-classes nil-value default-closed]}]
  (reagent/with-let [*open (reagent/cursor *open-rows [title])]
    (let [by-value (group-ids (if nil-value
                               #(or (value %) nil-value)
                               value)
                             selection)
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
              (for [value groups :let [ids (get by-value value)]]
                (when (and value (not-empty ids))
                  [tag/component
                   {:key value
                    :class (when add-classes (add-classes value))
                    
                    :count (count ids)
                    :body value
                    :close true
                    :on-select #(rf/dispatch [:select-ids ids])
                    
                    :on-close #(rf/dispatch [:deselect-ids ids])
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

(defn- base-cost [path-cost determinants model-mode x]
  (case (::candidate/type x)
      :path (path-cost x)
      :building (tariff/connection-cost
                 (document/connection-cost-for-id determinants (::tariff/cc-id x))
                 (candidate/annual-demand x model-mode)
                 (candidate/peak-demand x model-mode))
      nil))

(defn- solution-row-classes [x]
  ["solution"
   (cond
     (or (= x "network") (= x "impossible") (= x "peripheral"))
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
              ac (capital-mode (:capex (::solution/alternative %)))
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


(defn component
  "The panel in the bottom right which displays some information about the currently selected candidates."
  []
  (let [selection    @(rf/subscribe [:candidates/selected])
        has-solution @(rf/subscribe [:doc/has-solution])
        model-mode   @(rf/subscribe [:doc/mode])
        mode-name    (case model-mode :cooling "Cold" "Heat")
        cost-factors @(rf/subscribe [:doc/cost-factors])
        custom-keys  @(rf/subscribe [:candidates/selected-user-fields])]

    [:div.component--selection-info
     [:header.selection-header
      (cond (empty? selection)
            "No selection"
            (empty? (rest selection)) "One candidate selected"
            :else (str (count selection) " candidates selected"))]

     (let [base-cost (partial base-cost (document/path-cost-fn cost-factors)
                              cost-factors model-mode)]
       [:div.selection-table
        [chips-row selection "Type"
         (fn [c]
           (cond
             (candidate/has-supply? c) "supply"
             (= :building (::candidate/type c)) "demand"
             true (::candidate/type c)))

         ]

        (for [[field type] custom-keys]
          [:<> {:key field}
           (case type
             :number
             [number-row selection field (fn [x] (-> x ::candidate/user-fields (get field)))
              :summary :sum :unit "" :default-closed true]

             :bit
             [chips-row selection field
              (fn [x]
                (let [s (-> x ::candidate/user-fields (get field))]
                  (case s
                    1 "yes"
                    0 "no"
                    
                    nil)))
              :nil-value "None" :default-closed true]

             :boolean
             [chips-row selection field
              (fn [x]
                (let [s (-> x ::candidate/user-fields (get field))]
                  (case s
                    true "yes"
                    false "no"
                    nil)))
              :nil-value "None" :default-closed true]
             
             [chips-row selection field (fn [x]
                                (let [s (-> x ::candidate/user-fields (get field))]
                                  (if (string/blank? s) nil s)))
              
              :nil-value "None" :default-closed true])])
        
        [chips-row selection "Constraint" ::candidate/inclusion
         :nil-value "Forbidden" :add-classes (fn [x] ["constraint" (name x)])]
        [chips-row selection "Tariff"    (partial building-tariff-name cost-factors)]
        [chips-row selection "Edited"    (comp {false "no" true "yes" nil "no"} ::candidate/modified)]
        [chips-row selection "Profile"   (partial building-profile-name cost-factors)]
        [number-row selection "Market rate" ::solution/market-rate
         :summary :mean :unit "c/kWh" :scale 100]
        [chips-row selection "Civils"    (partial path-civil-cost-name cost-factors)]

        [number-row selection "Length" ::path/length :unit "m"]
        [number-row selection "Base cost" base-cost :unit "¤"
         :tooltip (str "For buildings this is the connection cost. "
                       "For paths it is the cost of the smallest pipe.")]

        [number-row selection (str mode-name " demand") #(candidate/annual-demand % model-mode)
         :unit "Wh/yr" :scale 1000]

        (when (and has-solution
                   (some #(not=
                           (candidate/solved-annual-demand % model-mode)
                           (candidate/annual-demand % model-mode))
                         selection))

          [number-row selection "Insulated demand" #(candidate/solved-annual-demand % model-mode)
           :tooltip "Demand after solving, including efect of insulation"
           :unit "Wh/yr" :scale 1000])
        
        [number-row selection (str mode-name " peak") #(candidate/peak-demand % model-mode)
         :tooltip (when has-solution "Peak demand input for optimisation model")
         :unit "Wp" :scale 1000]

        (when (and has-solution
                   (some #(not=
                           (candidate/solved-peak-demand % model-mode)
                           (candidate/peak-demand % model-mode))
                         selection))
          [number-row selection "System peak" #(candidate/solved-peak-demand % model-mode)
           :tooltip "Peak after solving, including peak adjustments from tank factor"
           :unit "Wp" :scale 1000])

        [linear-density-row selection model-mode]

        (when has-solution
          [:<>
           [chips-row selection "In solution" candidate/solution-description
            :nil-value "no" :add-classes solution-row-classes]
           [number-row selection "Coincidence" ::solution/diversity
            :summary :mean :unit "%" :scale 100]
           [number-row selection "Required capacity" ::solution/capacity-kw
            :summary :max :unit "W" :scale 1000]
           [number-row selection "Max. capacity" ::solution/max-capacity-kw
            :summary :max :unit "W" :scale 1000]
           [number-row selection "Diameter" ::solution/diameter-mm
            :summary :max :unit "m" :scale 0.001]
           [chips-row selection "Existing pipe"
            (fn [x] (when (and (::path/exists x) (::solution/included x))
                      (if (::solution/upgraded x) "upgraded" "kept")))]

           [cost-row selection]
           
           [number-row selection "Revenue" (comp :annual ::solution/heat-revenue)
            :unit "¤/yr"]
           
           [losses-row selection]
           ])])]))



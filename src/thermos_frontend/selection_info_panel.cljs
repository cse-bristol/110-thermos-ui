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
            ))

(declare component)

(defn- category-row [flow valuefn candidates & {:keys [add-classes]}]
  (let [by-value (group-by valuefn candidates)
        chips (remove nil?
                      (for [value (sort (keys by-value))
                            :let [candidates (get by-value value)]]
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
        ]
    chips))

(defn component
  "The panel in the bottom right which displays some information about the currently selected candidates."
  [flow]
  (reagent/with-let [capital-mode (reagent/atom :principal)]
    (let [model-mode @(flow/view* flow document/mode)
          has-solution @(flow/view* flow document/has-solution?)
          selected-candidates @(flow/view* flow operations/selected-candidates)
          document flow
          
          mode-name (case model-mode :cooling "Cold" "Heat")

          rsum (partial reduce +)
          rmax (partial reduce max)
          rmin (partial reduce min)
          rmean #(/ (rsum %) (count %))

          ;; TODO all these @document below will be rerendering this
          ;; bit when we drag or whatever, which is clearly silly
          
          base-cost #(case (::candidate/type %)
                       :path (document/path-cost % @document)
                       :building (tariff/connection-cost
                                  (document/connection-cost-for-id @document (::tariff/cc-id %))
                                  (candidate/annual-demand % model-mode)
                                  (candidate/peak-demand % model-mode))
                       nil)
          
          sc-class "selection-table-cell--tag-container"
          cat (fn [k u & {:keys [add-classes]}]
                (category-row flow #(or (k %) u) selected-candidates
                              :add-classes add-classes))

          num (fn [k agg unit & [scale]]
                (let [scale (or scale 1)
                      vals (remove nil? (map k selected-candidates))]
                  (when-not (empty? vals)
                    [:span
                     (when (seq (rest vals))
                       {:class :has-tt
                        :title (str
                                "Range: "
                                (si-number (* scale (reduce min vals))) unit
                                " — "
                                (si-number (* scale (reduce max vals))) unit)})
                     (si-number (* scale (agg vals))) unit]
                    )))
          ]
      [:div.component--selection-info
       [:header.selection-header
        (str
         (count selected-candidates)
         (if (= 1 (count selected-candidates)) " candidate" " candidates")
         " selected")]

       [:div.selection-table
        (for [[row-name class contents]
              [["Type" sc-class (cat ::candidate/type nil)]
               ["Category" sc-class (cat ::candidate/subtype "Unclassified")]
               ["Constraint" sc-class (cat ::candidate/inclusion "Forbidden"
                                           :add-classes
                                           (fn [x] ["constraint" (name x)]))]
               
               ["Name" sc-class (cat ::candidate/name "None")]
               [[:span "Tariff " [:span
                                  {:on-click #(swap! document view/switch-to-tariffs)
                                   :style {:cursor :pointer }
                                   }
                                  "👁"]]
                sc-class
                (cat
                 (fn [x]
                   (when (candidate/is-building? x)
                     (document/tariff-name @document (::tariff/id x))))
                 nil)]

               ["Profile"
                sc-class
                (cat
                 (fn [x]
                   (when (candidate/is-building? x)
                     (document/profile-name @document (::supply/profile-id x))))
                 nil)
                ]

               [[:span.has-tt
                 {:title
                  "For buildings on the market tariff, this is the unit rate offered. For multiple selection, it is the mean value."}
                 "Market rate"] nil
                (num ::solution/market-rate rmean "c/kWh" 100)]

               [[:span "Civils " [:span
                                  {:on-click #(swap! document view/switch-to-pipe-costs)
                                   :style {:cursor :pointer}}
                                  "👁"]]
                
                sc-class (cat
                          (fn [x]
                            (when (candidate/is-path? x)
                              (document/civil-cost-name
                               @document
                               (::path/civil-cost-id x))))
                          nil)]
               
               ["Length" nil (num ::path/length  rsum "m")]
               [[:span.has-tt
                 {:title
                  (str "For buildings this is the connection cost. "
                       "For paths it is the cost of a 10mm pipe.")}
                 "Base cost"] nil (num base-cost   rsum "¤")]
               
               [(str mode-name " demand") nil (num
                                               #(candidate/annual-demand % model-mode)
                                               rsum "Wh/yr" 1000)]
               [(str mode-name " peak") nil   (num
                                               #(candidate/peak-demand % model-mode)
                                               rsum "Wp" 1000)]

               [[:span.has-tt
                 {:title
                  "Linear density of the selected objects. If you want to see the linear density of a solution, select only the things in the solution."}
                 "Lin. density"]
                nil
                (let [total-kwh (reduce + 0 (keep
                                             #(or (::solution/kwh %)
                                                  (candidate/annual-demand % model-mode))
                                             selected-candidates))
                      total-m   (when (and total-kwh
                                           (pos? total-kwh))
                                  (reduce + 0 (keep ::path/length selected-candidates)))]
                  (when (and total-kwh total-m
                             (pos? total-kwh)
                             (pos? total-m))
                    [:span (si-number (* 1000 (/ total-kwh total-m))) "Wh/m"]))]
               ]]

          (when-not (empty? contents)
            [:div.selection-table__row {:key row-name}
             [:div.selection-table__cell.selection-table__cell--first-col row-name]
             [:div.selection-table__cell.selection-table__cell--second-col
              {:class class}
              contents]])
          )
        (when has-solution
          (for [[row-name class contents]
                [["In solution" sc-class
                  (cat #(cond
                          (candidate/is-connected? %) "network"
                          (candidate/got-alternative? %)
                          (-> %
                              (::solution/alternative)
                              (::supply/name))

                          (candidate/got-counterfactual? %)
                          (-> %
                              (::solution/alternative)
                              (::supply/name)
                              (str " (existing)"))
                          
                          (candidate/unreachable? %) "impossible")
                       
                       "no"
                       :add-classes
                       ;; TODO this is a bit ugly right now
                       (fn [x] ["solution"
                                (cond
                                  (or (= x "network") (= x "impossible"))
                                  x
                                  
                                  (= x "no") "no"
                                  
                                  (.endsWith x "(existing)")
                                  "no"
                                  
                                  true "individual")]))
                  ]
                 ["Coincidence" nil (num ::solution/diversity rmean "%" 100)]
                 ["Capacity"    nil (num ::solution/capacity-kw rmax "W" 1000)]
                 ["Diameter"    nil (num ::solution/diameter-mm rmax "m" 0.001)]

                 [[:span.has-tt
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
                     :value-atom capital-mode
                     :values
                     {:principal "Principal"
                      :present   "PV Capex"
                      :total     "Σ Capex"}
                     }]
                   
                   ]
                  nil
                  (num #(let [capital-mode @capital-mode
                              p  (capital-mode (::solution/pipe-capex %))
                              cc (capital-mode (::solution/connection-capex %))
                              sc (capital-mode (::solution/supply-capex %))
                              ac (capital-mode (::solution/alternative %))
                              ics (keep capital-mode (::solution/insulation %))
                              ic (when (seq ics) (reduce + 0 ics))
                              ]
                          (when (or p cc sc ac ic)
                            (+ p cc sc ac ic)))
                       rsum "¤")
                  ]

                 ["Revenue" nil (num (comp :annual ::solution/heat-revenue) rsum "¤/yr")]
                 ["Losses"
                  nil
                  (let [annual (num ::solution/losses-kwh rsum "Wh/yr" 1000)]
                    (when (seq annual)
                      (let [total-kwh
                            (reduce + 0 (keep ::solution/losses-kwh
                                              selected-candidates))

                            total-length
                            (reduce + 0 (keep
                                         #(when (::solution/losses-kwh %)
                                            (::path/length %))
                                         selected-candidates))]
                        [:span annual ", "
                         (si-number
                          (/ (* 1000 (annual-kwh->kw total-kwh)) total-length))
                         "W/m"
                         ])))
                  ]
                 ]]
            (when-not (empty? contents)
              [:div.selection-table__row {:key row-name}
               [:div.selection-table__cell.selection-table__cell--first-col row-name]
               [:div.selection-table__cell.selection-table__cell--second-col
                {:class class}
                contents]])
            ))
        ]
       ])))




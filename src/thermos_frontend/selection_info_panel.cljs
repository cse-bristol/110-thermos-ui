(ns thermos-frontend.selection-info-panel
  (:require [reagent.core :as reagent]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.document :as document]
            [thermos-specs.demand :as demand]
            [thermos-specs.tariff :as tariff]
            [thermos-specs.path :as path]
            [thermos-specs.solution :as solution]
            [thermos-specs.view :as view]
            [thermos-frontend.editor-state :as state]
            [thermos-frontend.operations :as operations]
            [thermos-frontend.tag :as tag]
            [thermos-frontend.format :refer [si-number local-format]]
            [thermos-util :refer [annual-kwh->kw]]
            [thermos-frontend.format :as format]
            [thermos-frontend.inputs :as inputs]
            ))

(declare component)

(defn- category-row [document valuefn candidates & {:keys [add-classes]}]
  (let [by-value (group-by valuefn candidates)
        chips (remove nil?
                      (for [[value candidates] by-value]
                        (when (and value (not-empty candidates))
                          [tag/component
                           {:key value
                            :class (when add-classes
                                     [add-classes (name value)])
                            
                            :count (count candidates)
                            :body value
                            :close true
                            :on-select #(state/edit! document
                                                     operations/select-candidates
                                                     (map ::candidate/id candidates)
                                                     :replace)
                            :on-close #(state/edit! document
                                                    operations/deselect-candidates
                                                    (map ::candidate/id candidates))
                            }])))
        ]
    chips))

(defn component
  "The panel in the bottom right which displays some information about the currently selected candidates."
  [document]
  (reagent/with-let [capital-mode (reagent/atom :principal)]    
    (let [rsum (partial reduce +)
          rmax (partial reduce max)
          rmin (partial reduce min)
          rmean #(/ (rsum %) (count %))

          base-cost #(case (::candidate/type %)
                       :path (document/path-cost % @document)
                       :building (tariff/connection-cost
                                  (document/connection-cost-for-id @document (::tariff/cc-id %))
                                  (::demand/kwh %)
                                  (::demand/kwp %))
                       nil)
          
          has-solution (document/has-solution? @document)
          selected-candidates (operations/selected-candidates @document)
          
          sc-class "selection-table-cell--tag-container"
          cat (fn [k u & {:keys [add-classes]}]
                (category-row document #(or (k %) u) selected-candidates
                              :add-classes add-classes))

          num (fn [k agg unit & [scale]]
                (let [scale (or scale 1)
                      vals (remove nil? (map k selected-candidates))]
                  (when-not (empty? vals)
                    (if (seq (rest vals))
                      [:span.has-tt
                       {:title
                        (str
                         "Range: "
                         (si-number (* scale (reduce min vals)))
                         " — "
                         (si-number (* scale (reduce max vals)))
                         unit)}
                       
                       (si-number (* scale (agg vals)))
                       unit
                       ]
                      [:span (si-number (* scale (agg vals))) unit])
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
               ["Classification" sc-class (cat ::candidate/subtype "Unclassified")]
               ["Constraint" sc-class (cat ::candidate/inclusion "Forbidden"
                                           :add-classes "constraint")]
               
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

               [[:span.has-tt
                 {:title
                  "For buildings on the market tariff, this is the unit rate offered. For multiple selection, it is the mean value."}
                 "Market rate"] nil
                (num ::solution/market-rate rmean "c/kWh" 100)
                ]

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
               
               ["Demand" nil (num ::demand/kwh  rsum "Wh/yr" 1000)]
               ["Peak" nil (num ::demand/kwp  rsum "Wp" 1000)]]]

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
                          (candidate/got-alternative? %) "individual"
                          (candidate/unreachable? %) "impossible")
                       
                       "no"
                       :add-classes "solution")
                  ]
                 ["Coincidence"
                  nil
                  (num ::solution/diversity rmean "%" 100)
                  ]
                 ["Capacity"
                  nil
                  (num ::solution/capacity-kw rmax "W" 1000)
                  ]
                 ["Diameter"
                  nil
                  (num ::solution/diameter-mm rmax "m" 0.001)
                  ]

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

                 ["Revenue"
                  nil
                  (num (comp :annual ::solution/heat-revenue) rsum "¤/yr")
                  ]
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




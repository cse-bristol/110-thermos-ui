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
                  [:span (si-number (* scale (agg vals))) unit])))
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
                   (document/tariff-name
                    @document
                    (::tariff/id x))))
               nil)]

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
                 {:title "This includes network supply, connection costs, insulation costs and individual system costs"}
                 "Principal"]
                nil
                (num #(let [p  (:principal (::solution/pipe-capex %) 0)
                            cc (:princpal  (::solution/connection-capex %) 0)
                            sc (:princpal  (::solution/supply-capex %) 0)
                            ac (:princpal  (::solution/alternative %) 0)
                            ic (reduce + 0 (keep :principal (::solution/insulation %)))
                            t (+ p cc sc ac ic)
                            ]
                        (when-not (zero? t) t))
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
                    [:span annual ", " (num (fn [p]
                                              (/ (annual-kwh->kw (::solution/losses-kwh p))
                                                 (::path/length p)))
                                            rsum "W/m" 1000)]))
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
     ]))




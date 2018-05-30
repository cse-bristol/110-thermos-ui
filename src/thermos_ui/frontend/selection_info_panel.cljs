(ns thermos-ui.frontend.selection-info-panel
  (:require [reagent.core :as reagent]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.specs.solution :as solution]
            [thermos-ui.frontend.editor-state :as state]
            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.frontend.inclusion-selector :as inclusion-selector]
            [thermos-ui.frontend.tag :as tag]))

(declare component)

(defn- category-row [document valuefn candidates]
  (let [by-value (group-by valuefn candidates)
        chips (remove nil?
                      (for [[value candidates] by-value]
                        (when (and value (not-empty candidates))
                          [tag/component
                           {:key value
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
  (let [has-solution (::solution/solution @document)
        selected-candidates (operations/selected-candidates @document)
        sc-class "selection-table-cell--tag-container"
        cat (fn [k u]
              (category-row document #(or (k %) u) selected-candidates))

        num (fn [k agg unit]
              (let [vals (remove nil? (map k selected-candidates))]
                (when-not (empty? vals)
                  [:span (str
                          (.toLocaleString
                           (.toPrecision
                            (apply agg vals)
                            3))
                          unit)])))
        ]
    [:div.component--selection-info
     [:header.selection-header
      (str
       (count selected-candidates)
       (if (= 1 (count selected-candidates)) " candidate" " candidates")
       " selected")]

     [:div.selection-table
      (for [[row-name class contents]
            [
             ["Type" sc-class (cat ::candidate/type nil)]
             ["Classification" sc-class (cat ::candidate/subtype "Unclassified")]
             ["Constraint" sc-class (cat ::candidate/inclusion "Forbidden")]
             ["Name" sc-class (cat ::candidate/name "None")]
             (when has-solution
               ["In solution" sc-class (cat 
                                     #(and (-> % ::solution/candidate ::solution/included) "yes") "no")])
             
             ["Length" nil (num ::candidate/length  + " m")]
             ["Cost" nil (num ::candidate/path-cost +
                              " ¤"
                              )]
             ["Demand" nil (num ::candidate/demand  + " kWh/yr")]

             (when has-solution
               ["Heat flow"
                nil
                (num (comp ::solution/heat-flow ::solution/candidate)
                     + "MW")
                ])]]

        (when-not (empty? contents)
          [:div.selection-table__row {:key row-name}
           [:div.selection-table__cell.selection-table__cell--first-col row-name]
           [:div.selection-table__cell.selection-table__cell--second-col
            {:class class}
            contents]]))
]
     ]))



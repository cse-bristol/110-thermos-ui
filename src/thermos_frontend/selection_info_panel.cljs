(ns thermos-frontend.selection-info-panel
  (:require [reagent.core :as reagent]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.document :as document]
            [thermos-specs.demand :as demand]
            [thermos-specs.path :as path]
            [thermos-specs.solution :as solution]
            [thermos-frontend.editor-state :as state]
            [thermos-frontend.operations :as operations]
            [thermos-frontend.inclusion-selector :as inclusion-selector]
            [thermos-frontend.tag :as tag]
            [thermos-frontend.format :refer [si-number]]
            [thermos-util :refer [annual-kwh->kw]]
            ))

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
  (let [rsum (partial reduce +)
        rmax (partial reduce max)
        rmin (partial reduce min)
        rmean #(/ (rsum %) (count %))

        base-cost #(case (::candidate/type %)
                     :path (document/path-cost % @document)
                     :building (when-let [cc (::demand/connection-cost %)]
                                 (and (not (zero? cc))
                                      (* cc (::demand/kwp % 0))))
                     nil)
        
        has-solution (document/has-solution? @document)
        selected-candidates (operations/selected-candidates @document)
        selected-technologies (mapcat (comp ::solution/technologies ::solution/candidate)
                                      selected-candidates)
        sc-class "selection-table-cell--tag-container"
        cat (fn [k u]
              (category-row document #(or (k %) u) selected-candidates))

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
             ["Constraint" sc-class (cat ::candidate/inclusion "Forbidden")]
             ["Name" sc-class (cat ::candidate/name "None")]
             
             ["Length" nil (num ::path/length  rsum "m")]
             [[:span
               {:title
                "For buildings this is the connection cost. "
                "For paths it is the cost of a 10mm pipe."}
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
              [["In solution" sc-class (cat #(when (candidate/in-solution? %) "yes") "no")]
               ["Diversity"
                nil
                (num ::solution/diversity rmean "")
                ]
               ["Capacity"
                nil
                (num ::solution/capacity-kw rmax "W" 1000)
                ]
               ["Diameter"
                nil
                (num ::solution/diameter-mm rmax "m" 0.001)
                ]
               ["Principal"
                nil
                (num ::solution/principal rsum "¤")
                ]
               ["Revenue"
                nil
                (num ::solution/heat-revenue rsum "¤/yr")
                ]
               ["Losses"
                nil
                (let [annual (num ::solution/losses-kwh rsum "Wh/yr" 1000)]
                  (when (seq annual)
                    [:span annual ", " (num (fn [p]
                                              (/ (annual-kwh->kw (::solution/losses-kwh %))
                                                 (::path/length t)))
                                            rsum "W" 1000)]))
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




(ns thermos-ui.frontend.selection-info-panel
  (:require [reagent.core :as reagent]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.frontend.editor-state :as state]
            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.frontend.inclusion-selector :as inclusion-selector]
            [thermos-ui.frontend.tag :as tag]))

(declare component row-types)

(defn component
  "The panel in the bottom right which displays some information about the currently selected candidates."
  [document]
  (let [selected-candidates (operations/selected-candidates @document)]
    [:div.component--selection-info

     [:header.selection-header
      (str
       (count selected-candidates)
       (if (= 1 (count selected-candidates)) " candidate" " candidates")
       " selected")]

     [:div.selection-table
      (for [{row-name :row-name f :get-row-content} (row-types document)]
        (let [row-content (f selected-candidates)]
          (when-not (empty? row-content)
            [:div.selection-table__row {:key row-name}
             [:div.selection-table__cell.selection-table__cell--first-col row-name]
             [:div.selection-table__cell.selection-table__cell--second-col
              (if (contains? #{"Type" "Classification" "Constraint" "Name"} row-name)
                {:class "selection-table-cell--tag-container"})
              row-content]])))]
     ]))

(defn row-types
  "Define a spec for all the rows to be displayed.
   Each row will have:
     :row-name The heading for the row
     :get-row-content A function to fetch the content of the row, given a list of candidates."
  [document]
  (let [filter-by-property (fn [k v] (fn [] (let [filtered-candidates
                                                           (filter
                                                            (fn [cand] (= (get cand k) v))
                                                            (operations/selected-candidates @document))]
                                                       (state/edit! document
                                                                    operations/select-candidates
                                                                    (map ::candidate/id filtered-candidates)
                                                                    :replace))))]
    [{:row-name "Type"
      :get-row-content (fn [candidates]
                         (let [by-type (group-by ::candidate/type candidates)]
                           (for [[type candidates] by-type]
                             (let [type (or type "Unknown")]
                               [tag/component {:key type
                                               :count (count candidates)
                                               :body (str type)
                                               :close true
                                               :on-select (filter-by-property ::candidate/type type)
                                               :on-close
                                               #(state/edit! document operations/deselect-candidates (map ::candidate/id candidates))
                                               }]))))}

     {:row-name "Classification"
      :get-row-content (fn [candidates]
                         (let [by-type (group-by ::candidate/subtype candidates)]
                           (for [[type candidates] by-type]
                             [tag/component {:key type
                                             :count (count candidates)
                                             :body (str type)
                                             :close true
                                             :on-select (filter-by-property ::candidate/subtype type)
                                             :on-close
                                             #(state/edit! document operations/deselect-candidates (map ::candidate/id candidates))
                                             }])))}

     {:row-name "Constraint"
      :get-row-content (fn [candidates]
                         (let [by-constraint (group-by ::candidate/inclusion candidates)]
                           (for [[constraint candidates] by-constraint]
                             (let [constraint (or constraint "- None -")]
                               [tag/component {:key constraint
                                               :count (count candidates)
                                               :body (name constraint)
                                               :close true
                                               :on-select (filter-by-property ::candidate/inclusion constraint)
                                               :on-close
                                               #(state/edit! document operations/deselect-candidates (map ::candidate/id candidates))}]))))}
     {:row-name "Name"
      :get-row-content (fn [candidates]
                         (let [by-name (group-by ::candidate/name candidates)]
                           (for [[name candidates] by-name]
                             (let [name (or name "unknown")]
                               [tag/component {:key name
                                               :count (count candidates)
                                               :body (str name)
                                               :close true
                                               :on-select (filter-by-property ::candidate/name name)
                                               :on-close
                                               #(state/edit! document operations/deselect-candidates (map ::candidate/id candidates))}]))))}
     {:row-name "Length"
      :get-row-content (fn [candidates]
                         (when-not (empty? candidates)
                           (str (reduce + 0 (map ::candidate/length candidates)) "m")))
      }
     {:row-name "Heat demand"
      :get-row-content (fn [candidates]
                         (when-not (empty? candidates)
                           (str (reduce + 0 (map ::candidate/demand candidates)) "kWh/year")))}

     ]))

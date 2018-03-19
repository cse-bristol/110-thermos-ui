(ns thermos-ui.frontend.network-candidates-filter
  (:require [reagent.core :as reagent]
            [thermos-ui.specs.view :as view]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.frontend.editor-state :as state]
            [thermos-ui.frontend.operations :as operations]))

(defn component
  [document
   {items :items
    key :key}]
  (fn []
  (let [filter-set (set (map key items))
        selected-filters (operations/get-table-filters @document key)]
      [:div.filter-dropdown {:on-click (fn [e] (.stopPropagation e))}
       "Filter:" [:br][:br]
       ;; If there are more than 5 options, add an "All" checkbox
       (if (> (count filter-set) 5)
         [:div {:style {:border-bottom "1px solid #ccc" :padding "4px 0" :margin-bottom "4px"}}
          [:label
           [:input {:type "checkbox"
                    :default-checked (= filter-set selected-filters)
                    :on-change (fn [e] (if e.target.checked
                                         (state/edit! document operations/add-table-filter-values key filter-set)
                                         (state/edit! document operations/remove-all-table-filter-values key)))}]
           "All"]])
       (map
        (fn [val]
          [:div {:key val}
           [:label
            [:input {:type "checkbox"
                     :checked (contains? selected-filters val)
                     :on-change (fn [e] (if e.target.checked
                                          (state/edit! document operations/add-table-filter-value key val)
                                          (state/edit! document operations/remove-table-filter-value key val)))}]
            (if (boolean? val)
              (if val "Yes" "No")
              val)]])
        filter-set)
       ])))

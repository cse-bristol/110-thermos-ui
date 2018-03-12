(ns thermos-ui.frontend.network-candidates-panel
  (:require [reagent.core :as reagent]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.frontend.editor-state :as state]
            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.frontend.virtual-table :as virtual-table]))

(declare component selected-cell-renderer)

(defn component
  "DOCSTRING"
  [document]
  [:div {:style {:height "100%"}}

   ;; TODO this will rerun whenever we modify anything in document we
   ;; need a cursor instead, maybe there should be some operations
   ;; things for this.
   (let [items (operations/included-candidates @document)]
     [virtual-table/component
      {:items items}
      ;; columns
      {:label "Selected"
       :key ::candidate/selected
       :cellRenderer (fn [args] (selected-cell-renderer document args))}
      {:label "Address" :key ::candidate/name}
      {:label "Type"
       :key ::candidate/type
       :cellRenderer (fn [arg]
                       (let [cell-value (.-cellData arg)]
                         (name cell-value)))}
      {:label "Postcode" :key ::candidate/postcode}
      ])])

(defn selected-cell-renderer
  "Custom cell renderer for `Selected` column.
  Puts in a checkbox allowing you to (de)select candidates."
  [document args]
  (let [is-selected (if args (.-cellData args) false)
        candidate-id (if args (::candidate/id (.. args -rowData)) false)]
    (reagent/as-element [:input
                         {:type "checkbox"
                          :default-checked is-selected
                          :on-change (fn [e]
                                       (if (.. e -target -checked)
                                         (state/edit! document
                                                      operations/select-candidates
                                                      [candidate-id]
                                                      :union)
                                         (state/edit! document
                                                      operations/deselect-candidates
                                                      [candidate-id])))}])))

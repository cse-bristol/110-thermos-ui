(ns thermos-ui.frontend.network-candidates-panel
  (:require [reagent.core :as reagent]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.specs.view :as view]
            [thermos-ui.frontend.editor-state :as state]
            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.frontend.network-candidates-filter :as network-candidates-filter]
            [thermos-ui.frontend.virtual-table :as virtual-table]))

(declare component filterable-header-renderer selected-cell-renderer)

(defn component
  "DOCSTRING"
  [document]
  [:div {:style {:height "100%"}}

   ;; TODO this will rerun whenever we modify anything in document we
   ;; need a cursor instead, maybe there should be some operations
   ;; things for this.
   (let [items (operations/included-candidates @document)]
     [virtual-table/component
      {:items items
       :filters (operations/get-all-table-filters @document)}
      ;; columns
      {:label "Selected"
       :key ::candidate/selected
       :cellRenderer (fn [args] (selected-cell-renderer document args))
       :headerRenderer (fn [args] (filterable-header-renderer document items ::candidate/selected args))}
      {:label "Address"
       :key ::candidate/name}
      {:label "Type"
       :key ::candidate/type
       :cellRenderer (fn [arg]
                       (name (.-cellData arg)))
       :headerRenderer (fn [args] (filterable-header-renderer document items ::candidate/type args))}
      {:label "Postcode" :key ::candidate/postcode
       :headerRenderer (fn [args] (filterable-header-renderer document items ::candidate/postcode args))}
      ])])

(defn filterable-header-renderer
  "Custom render function for column headers which need to be filterable."
  [doc items key args]
  (reagent/as-element
   (let [is-open (= (->> @doc ::view/view-state ::view/table-state ::view/open-filter) key)]
     [:span.ReactVirtualized__Table__headerTruncatedText {:style {:width "100%"}}
      (.-label args)
      [:span.filter-icon {:class (str "filter-icon"
                                      (if (not-empty (operations/get-table-filters @doc key))
                                        " filter-icon--is-filtered"))
                          :on-click (fn [e] (.stopPropagation e)
                                      (if is-open
                                        (state/edit! doc operations/close-table-filter)
                                        (do ;; If opening, need to do this hack so that the pop-up is visible
                                          (set! (..
                                                 (js/document.querySelector ".ReactVirtualized__Table__headerRow")
                                                 -style
                                                 -overflow)
                                            "initial")
                                          (state/edit! doc operations/open-table-filter key))))
                          }]
      (if is-open
        [network-candidates-filter/component doc {:items items :key key}])
      ])))

(defn selected-cell-renderer
  "Custom cell renderer for `Selected` column.
  Puts in a checkbox allowing you to (de)select candidates."
  [document args]
  (let [is-selected (if args (.-cellData args) false)
        candidate-id (if args (::candidate/id (.. args -rowData)) false)]
    (reagent/as-element [:input
                         {:type "checkbox"
                          :key candidate-id
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

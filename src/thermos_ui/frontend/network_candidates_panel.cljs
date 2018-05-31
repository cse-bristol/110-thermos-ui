(ns thermos-ui.frontend.network-candidates-panel
  (:require [goog.object :as o]
            [reagent.core :as reagent]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.specs.solution :as solution]
            [thermos-ui.specs.view :as view]
            [thermos-ui.frontend.editor-state :as state]
            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.frontend.network-candidates-filter :as network-candidates-filter]
            [thermos-ui.frontend.virtual-table :as virtual-table]))

(declare component filterable-header-renderer selected-cell-renderer)

(defn component
  "The panel showing all the candidates which can (or must) be required in the network."
  [document]
  (reagent/with-let
    [solution (reagent/cursor document [::solution/solution])
     all-filters (reagent/track #(operations/get-all-table-filters @document))
     num-applied-filters (reagent/track
                          #(reduce-kv (fn [init filter-key filter-value]
                                        (if (not-empty filter-value)
                                          (inc init)
                                          init))
                                      0 @all-filters))
     items (reagent/track #(operations/included-candidates @document))
     open-filter (reagent/cursor document [::view/view-state
                                           ::view/table-state
                                           ::view/open-filter])
     
     filtered-candidates (reagent/track #(operations/get-filtered-candidates @document))
     count-filtered-items (reagent/atom nil)]

    [:div {:class
           (str
                   "network-candidates-panel__virtual-table-container"
                   (if-not (zero? @num-applied-filters) " network-candidates-panel__virtual-table-container--filters"))}

     (let [data-value (fn [arg] (o/get arg "cellData"))
           data-name (fn [arg] (name (data-value arg)))

           open-filter @open-filter
           filtered-candidates @filtered-candidates
           items @items
           all-filters @all-filters
           
           col
           (fn [label key type cell-renderer]
             {:label label :key key
              :cellRenderer cell-renderer
              :headerRenderer
              (fn [args] (filterable-header-renderer
                          document
                          open-filter
                          filtered-candidates
                          items
                          key
                          args
                          type))}
             )
           ]
       [virtual-table/component
        {:items items
         :filters all-filters}
        ;; For the little filter summary box below we need the virtual table
        ;; to tell us how many items have been filtered.
        count-filtered-items
        ;; columns
        (col "Selected" ::candidate/selected "checkbox"
             (partial selected-cell-renderer document))
        (col "Name" ::candidate/name "text" data-value)
        (col "Type" ::candidate/type "checkbox" data-name)
        (col "Classification" ::candidate/subtype "checkbox" data-name)
        (when @solution
          {:label "Solution" :key [::solution/candidate ::solution/included]
           :cellRenderer (comp str data-value)
           :headerRenderer
           (fn [args] (filterable-header-renderer
                       document
                       open-filter
                       filtered-candidates
                       items
                       (comp ::solution/included ::solution/candidate)
                       args
                       "checkbox"))}
          )

        ])
     ;; Box displaying a summary of the filters if any have been applied
     (if-not (zero? @num-applied-filters)
       [:div.network-candidates-panel__filter-summary
        (str @num-applied-filters
             (if (> @num-applied-filters 1) " filters" " filter")
             " applied, showing " @count-filtered-items " of "
             (count @items) " candidates.")
        [:button.button.button--small
         {:on-click #(doseq [k (keys @all-filters)]
                       (state/edit! document operations/remove-all-table-filter-values k))}
         "CLEAR FILTERS"]])
     ]))

(defn filterable-header-renderer
  "Custom render function for column headers which need to be filterable."
  [doc open-filter filtered-candidates items key args type]
  (reagent/as-element
   (let [is-open (= open-filter key)]
     [:span.ReactVirtualized__Table__headerTruncatedText
     ;; This is quick and dirty - put a checkbox in the Selected column header to (de)select all
      (if (and (= key ::candidate/selected) (not-empty items))
        (let [all-selected? (= (count filtered-candidates)
                               (count (filter #(::candidate/selected %) filtered-candidates)))]
          [:input {:type "checkbox"
                   :style {:position "relative" :top "1px"}
                   :title (if all-selected? "Deselect all" "Select all")
                   :checked all-selected?
                   :on-click #(.stopPropagation %)
                   :on-change (fn [e]
                                (if (.. e -target -checked)
                                  ;; Select all
                                  (state/edit! doc
                                               operations/select-candidates
                                               (map ::candidate/id filtered-candidates)
                                               :union)
                                  ;; Deselect all
                                  (state/edit! doc
                                               operations/deselect-candidates
                                               (map ::candidate/id filtered-candidates))
                                  ))}])
        )
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
                                            "visible")
                                          (state/edit! doc operations/open-table-filter key))))
                          }]
      (when is-open
        [network-candidates-filter/component doc {:items items :key key} type])
      ])))

(defn selected-cell-renderer
  "Custom cell renderer for `Selected` column.
  Puts in a checkbox allowing you to (de)select candidates."
  [document args]
  (let [is-selected (if args (o/get args "cellData" false) false)
        candidate-id (if args (::candidate/id (o/get args "rowData" nil)) false)]
    (reagent/as-element [:input
                         {:type "checkbox"
                          :key candidate-id
                          :checked is-selected
                          :on-change (fn [e]
                                       (if (.. e -target -checked)
                                         (state/edit! document
                                                      operations/select-candidates
                                                      [candidate-id]
                                                      :union)
                                         (state/edit! document
                                                      operations/deselect-candidates
                                                      [candidate-id])))}])))

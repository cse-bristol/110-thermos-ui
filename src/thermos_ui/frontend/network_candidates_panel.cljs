(ns thermos-ui.frontend.network-candidates-panel
  (:require [goog.object :as o]
            [reagent.core :as reagent]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.specs.solution :as solution]
            [thermos-ui.specs.view :as view]
            [thermos-ui.frontend.editor-state :as state]
            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.frontend.network-candidates-filter :as network-candidates-filter]
            [thermos-ui.frontend.virtual-table :as virtual-table]
            [thermos-ui.frontend.format :refer [si-number]]
            ))

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
     candidates (reagent/track #(operations/included-candidates @document))
     open-filter (reagent/cursor document [::view/view-state
                                           ::view/table-state
                                           ::view/open-filter])
     
     filtered-candidates (reagent/track #(operations/get-filtered-candidates @document))
     ]

    
    (let [data-value (fn [arg] (o/get arg "cellData"))
          data-name (fn [arg]
                      (if-let [v (data-value arg)]
                        (name v))
                      )

          open-filter @open-filter
          filtered-candidates @filtered-candidates
          candidates @candidates
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
                         candidates
                         key
                         args
                         type))}
            )
          ]
      
      [:div.network-candidates-panel__virtual-table-container
       {:class
        (when-not (zero? @num-applied-filters)
          "network-candidates-panel__virtual-table-container--filters"
          )
        }

       [virtual-table/component
        {:items filtered-candidates}
        ;; columns
        (assoc (col "" ::candidate/selected "checkbox"
                    (partial selected-cell-renderer document))
               :width 70
               :flexShrink 0
               :flexGrow 0
               )
        (assoc (col "Name" ::candidate/name "text" data-value)
               :flexGrow 1
               :width 80)
        (assoc (col "Wh/yr" ::candidate/demand "number"
                    #(when-let [v (data-value %)]
                       (si-number (* 1000 v))))
               :width 90)
        (assoc (col "¤" ::candidate/path-cost "number"
                    #(when-let [v (data-value %)]
                       (si-number v)))
               :width 80)
        
        (col "Type" ::candidate/type "checkbox" data-name)
        
        (assoc (col "Class" ::candidate/subtype "checkbox" data-name)
               :width 120)
        (when @solution
          {:label "In?" :key [::solution/candidate ::solution/included]
           :cellRenderer #(if (data-value %)
                            "✓" "❌")
           :style #js {"text-align" "right"}
           :width 70
           :flexShrink 0
           :flexGrow 0
           :headerRenderer
           (fn [args] (filterable-header-renderer
                       document
                       open-filter
                       filtered-candidates
                       candidates
                       [::solution/candidate ::solution/included]
                       args
                       "checkbox"))}
          )
        

        ]

       (if-not (zero? @num-applied-filters)
         [:div.network-candidates-panel__filter-summary
          (str @num-applied-filters
               (if (> @num-applied-filters 1) " filters" " filter")
               " applied, showing " (count filtered-candidates) " of "
               (count candidates) " candidates.")
          [:button.button.button--small
           {:on-click #(state/edit! document operations/clear-filters)}
           "CLEAR FILTERS"]])
       ]
      )
    ;; Box displaying a summary of the filters if any have been applied
    
    ))

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

(ns thermos-frontend.network-candidates-panel
  (:require [goog.object :as o]
            [reagent.core :as reagent]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.cooling :as cooling]
            [thermos-specs.path :as path]
            [thermos-specs.solution :as solution]
            [thermos-specs.document :as document]
            [thermos-specs.view :as view]
            [thermos-frontend.editor-state :as state]
            [thermos-frontend.operations :as operations]
            [thermos-frontend.network-candidates-filter :as network-candidates-filter]
            [thermos-frontend.virtual-table :as virtual-table]
            [thermos-frontend.format :refer [si-number]]
            [thermos-frontend.flow :as f]
            [thermos-util :refer [count-if]]
            ))

(declare component filterable-header-renderer selected-cell-renderer)

(defn- count-filters [filters]
  (count-if (vals filters) not-empty))

(defn component
  "The panel showing all the candidates which can (or must) be required in the network."
  [flow]
  (let [solution            (f/view* flow document/has-solution?)
        filters             (f/view* flow operations/get-all-table-filters)
        n-filters           (f/view* filters count-filters)
        candidates          (f/view* flow operations/included-candidates)
        mode                (f/view* flow document/mode)
        filtered-candidates (f/view*
                             flow
                             ;; this is not so efficient
                             ;; we fill re-filter the candidates
                             ;; more than we need
                             operations/get-filtered-candidates)
        open-filter         (f/view*
                             flow
                             get-in
                             [::view/view-state
                              ::view/table-state
                              ::view/open-filter])

        pipe-cost-function
        (let [params (f/view*
                      flow
                      select-keys
                      [::document/mechanical-cost-per-m
                       ::document/mechanical-cost-per-m2
                       ::document/mechanical-cost-exponent
                       ::document/civil-cost-exponent
                       ::document/civil-costs
                       ])]
          (fn [path]
            (document/path-cost path @params)))
        
        ]
    (let [data-value (fn [arg] (o/get arg "cellData"))
          data-name  (fn [arg]
                       (if-let [v (data-value arg)]
                         (name v))
                       )

          [demand-key peak-key]
          (case @mode
            :cooling [::cooling/kwh ::cooling/kwp]
            [::demand/kwh ::demand/kwp])
          
          open-filter         @open-filter
          filtered-candidates @filtered-candidates
          candidates          @candidates
          filters             @filters
          
          col
          (fn [label key type cell-renderer]
            {:label        label :key key
             :cellRenderer cell-renderer
             :headerRenderer
             (fn [args] (filterable-header-renderer
                         flow
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
        (when-not (zero? @n-filters)
          "network-candidates-panel__virtual-table-container--filters"
          )
        }

       [virtual-table/component
        {:items filtered-candidates}
        ;; columns
        (assoc (col "" ::candidate/selected "checkbox"
                    (partial selected-cell-renderer flow))
               :width 70
               :flexShrink 0
               :flexGrow 0)
        
        (assoc (col "Name" ::candidate/name "text" data-value)
               :flexGrow 1
               :width 80)
        (assoc (col "Wh/yr" demand-key "number"
                    #(when-let [v (data-value %)]
                       (si-number (* 1000 v))))
               :width 90)
        (assoc (col "Wp" peak-key "number"
                    #(when-let [v (data-value %)]
                       (si-number (* 1000 v))))
               :width 90)
        (assoc (col "¤" ::path/cost-per-m "number"
                    #(when-let [v (data-value %)]
                       (si-number v)))
               :cellDataGetter
               #(pipe-cost-function (o/get % "rowData" nil))
               :width 80)
        
        (col "Type" ::candidate/type "checkbox" data-name)
        
        (assoc (col "Class" ::candidate/subtype "checkbox" data-name)
               :width 120)
        (when @solution
          {:label        "In?" :key ::solution/included
           :cellRenderer #(if (data-value %)
                            "✓" "❌")
           :style        #js {"textAlign" "right"}
           :width        70
           :flexShrink   0
           :flexGrow     0
           :headerRenderer
           (fn [args] (filterable-header-renderer
                       flow
                       open-filter
                       filtered-candidates
                       candidates
                       ::solution/included
                       args
                       "checkbox"))}
          )
        

        ]

       (let [n-filters @n-filters]
         (if-not (zero? n-filters)
           [:div.network-candidates-panel__filter-summary
            (str n-filters
                 (if (> n-filters 1) " filters" " filter")
                 " applied, showing " (count filtered-candidates) " of "
                 (count candidates) " candidates.")
            [:button.button.button--small
             {:on-click #(f/fire! flow [operations/clear-filters])}
             "CLEAR FILTERS"]]))
       ]
      )
    )
  )

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
                                  (f/fire! doc
                                           [operations/select-candidates
                                            (map ::candidate/id filtered-candidates)
                                            :union])
                                  ;; Deselect all
                                  (f/fire! doc
                                           [operations/deselect-candidates
                                            (map ::candidate/id filtered-candidates)])
                                  ))}])
        )
      (.-label args)
      [:span.filter-icon {:class (str "filter-icon"
                                      (if (not-empty @(f/view* doc operations/get-table-filters key))
                                        " filter-icon--is-filtered"))
                          :on-click (fn [e] (.stopPropagation e)
                                      (if is-open
                                        (f/fire! doc [operations/close-table-filter])
                                        (do ;; If opening, need to do this hack so that the pop-up is visible
                                          (set! (..
                                                 (js/document.querySelector ".ReactVirtualized__Table__headerRow")
                                                 -style
                                                 -overflow)
                                                "visible")
                                          (f/fire! doc [operations/open-table-filter key]))))
                          }]
      (when is-open
        [network-candidates-filter/component
         doc {:items items :key key} type])
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
                                         (f/fire! document
                                                  [operations/select-candidates
                                                   [candidate-id]
                                                   :union])
                                         (f/fire! document
                                                  [operations/deselect-candidates
                                                   [candidate-id]])))}])))

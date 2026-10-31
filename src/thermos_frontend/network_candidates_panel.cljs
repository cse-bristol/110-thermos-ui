;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

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
            [re-frame.core :as rf]
            [thermos-frontend.network-candidates-filter :as network-candidates-filter]
            [thermos-frontend.virtual-table :as virtual-table]
            [thermos-frontend.format :refer [si-number]]
            ))

(declare component filterable-header-renderer selected-cell-renderer)

(defn component
  "The panel showing all the candidates which can (or must) be required in the network."
  []
  (let [solution            (rf/subscribe [:doc/has-solution])
        n-filters           (rf/subscribe [:table/n-filters])
        candidates          (rf/subscribe [:candidates/included])
        mode                (rf/subscribe [:doc/mode])
        filtered-candidates (rf/subscribe [:table/filtered-candidates])
        open-filter         (rf/subscribe [:table/open-filter])
        pipe-costs          @(rf/subscribe [:doc/pipe-costs])
        pipe-cost-function  (document/path-cost-fn {::document/pipe-costs pipe-costs})]
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
          
          col
          (fn [label key type cell-renderer]
            {:label        label :key key
             :cellRenderer cell-renderer
             :headerRenderer
             (fn [args] (filterable-header-renderer
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
                    selected-cell-renderer)
               :width 70
               :flexShrink 0
               :flexGrow 0)
        
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
             {:on-click #(rf/dispatch [:table/clear-filters])}
             "CLEAR FILTERS"]]))
       ]
      )
    )
  )

(defn- filterable-header
  "A column header which can be filtered, as a component, so the
  subscription in it works."
  [open-filter filtered-candidates items key label type]
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
                                 (rf/dispatch [:select-ids (map ::candidate/id filtered-candidates) :union])
                                 ;; Deselect all
                                 (rf/dispatch [:deselect-ids (map ::candidate/id filtered-candidates)])
                                 ))}])
       )
     label
     [:span.filter-icon {:class (str "filter-icon"
                                     (if (not-empty @(rf/subscribe [:table/filter key]))
                                       " filter-icon--is-filtered"))
                         :on-click (fn [e] (.stopPropagation e)
                                     (if is-open
                                       (rf/dispatch [:table/close-filter])
                                       (do ;; If opening, need to do this hack so that the pop-up is visible
                                         (set! (..
                                                (js/document.querySelector ".ReactVirtualized__Table__headerRow")
                                                -style
                                                -overflow)
                                               "visible")
                                         (rf/dispatch [:table/open-filter key]))))
                         }]
     (when is-open
       [network-candidates-filter/component {:items items :key key} type])
     ]))

(defn filterable-header-renderer
  "Custom render function for column headers which need to be filterable."
  [open-filter filtered-candidates items key args type]
  (reagent/as-element
   [filterable-header open-filter filtered-candidates items key (.-label args) type]))

(defn selected-cell-renderer
  "Custom cell renderer for `Selected` column.
  Puts in a checkbox allowing you to (de)select candidates."
  [args]
  (let [is-selected (if args (o/get args "cellData" false) false)
        candidate-id (if args (::candidate/id (o/get args "rowData" nil)) false)]
    (reagent/as-element [:input
                         {:type "checkbox"
                          :key candidate-id
                          :checked (boolean is-selected)
                          :on-change (fn [e]
                                       (if (.. e -target -checked)
                                         (rf/dispatch [:select-ids [candidate-id] :union])
                                         (rf/dispatch [:deselect-ids [candidate-id]])))}])))

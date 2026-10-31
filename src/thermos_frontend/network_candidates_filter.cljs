;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.network-candidates-filter
  (:require [reagent.core :as reagent]
            [re-frame.core :as rf]
            [thermos-frontend.format :refer [si-number]]))

(defn component
  [{items :items key :key}
   type]
  (let [keyfn (if (seqable? key) #(get-in % key) #(get % key))
        filter-set (set (map keyfn items))
        selected-filters @(rf/subscribe [:table/filter key])]
    (case type
      ;; Searchable text field filter
      ;; @TODO put these inline styles into CSS file
      "text"
      (let [timeout (atom nil)] ;; To be used to in the key-up callback below
        [:div.filter-dropdown {:style {:width "200px"} :on-click (fn [e] (.stopPropagation e))}
         [:div.filter-text-input__container
          [:input.text-input.filter-text-input
           {:type "text"
            :placeholder "Type to filter"
            :default-value (or selected-filters "")
            :ref (fn [node] (if node (.focus node)))
            :on-key-press #(.stopPropagation %) ;; Prevent keyboard shortcuts from executing when you type
            :on-key-down (fn [e]
                           ;; Stop table from sorting automatically when you press space or enter.
                           (if (or (= e.key " ") (= e.key "Enter"))
                             (.stopPropagation e)))
            :on-key-up (fn [e]
                         ;; Allows us to use the event in the callback below
                         (.persist e)

                         ;; Use a timeout to wait 0.2s after user has stopped typing before
                         ;; initiating the filter.
                         (js/clearTimeout @timeout)
                         (reset! timeout (js/setTimeout
                                          (fn []
                                            (rf/dispatch [:table/add-filter-value key e.target.value])) 200))
                         )}]
          [:button.filter-text-input__clear-button
           {:on-click (fn [e]
                        (set! e.target.previousSibling.value "")
                        (rf/dispatch [:table/remove-filter key]))}
           "×"]]])

      ;; Checkbox filter
      "checkbox"
      [:div.filter-dropdown {:on-click (fn [e] (.stopPropagation e))}
       "Filter:" [:br][:br]
       ;; If there are more than 5 options, add an "All" checkbox
       (if (> (count filter-set) 5)
         [:div {:style {:border-bottom "1px solid #ccc" :padding "4px 0" :margin-bottom "4px"}}
          [:label
           [:input {:type "checkbox"
                    :default-checked (= filter-set selected-filters)
                    :on-change (fn [e] (if e.target.checked
                                         (rf/dispatch [:table/add-filter-values key filter-set])
                                         (rf/dispatch [:table/remove-filter key])))}]
           "All"]])
       (map
        (fn [val]
          [:div {:key val}
           [:label
            [:input {:type "checkbox"
                     :checked (contains? selected-filters val)
                     :on-change (fn [e] (if e.target.checked
                                          (rf/dispatch [:table/add-filter-value key val])
                                          (rf/dispatch [:table/remove-filter-value key val])))}]
            (if (or (nil? val) (boolean? val))
              (if val "Yes" "No")
              val)]])
        filter-set)
       ]
      "number"
      [:div.filter-dropdown {:on-click (fn [e] (.stopPropagation e))
                             :style {:overflow :hidden}}
       (let [lb (apply min (filter number? filter-set))
             ub (apply max (filter number? filter-set))
             
             [s0 s1] (or selected-filters [lb ub])
             [s0 s1] [(min s0 s1) (max s0 s1)]
             ]

         [:div {:style {:width :200px}}
          [:div (str (si-number s0) " - " (si-number s1))]
          
          [:input.double-range {:type :range
                                :value s0
                                :on-change
                                #(rf/dispatch [:table/set-filter key [(js/parseFloat (.. % -target -value)) s1]])
                                
                                :min lb
                                :max ub}]
          
          [:input.double-range {:type :range
                                :value s1
                                :on-change
                                #(rf/dispatch [:table/set-filter key [s0 (js/parseFloat (.. % -target -value))]])
                                :min lb
                                :max ub}]
          [:br][:br]
          [:button
           {:on-click
            #(rf/dispatch [:table/remove-filter key])
            }
           "Clear"]]
         )
       
       
       
       ]
      
      )))

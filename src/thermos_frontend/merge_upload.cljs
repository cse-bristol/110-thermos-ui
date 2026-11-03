;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.merge-upload
  "Dialog box for merging settings from one problem into another problem."  
  (:require [thermos-frontend.util :refer [upload-file]]
            [thermos-frontend.popover :as popover]
            [reagent.core :as reagent]
            [thermos-frontend.inputs :as inputs]
            [re-frame.core :as rf]
            [thermos-specs.parameters :as parameters]
            [clojure.string :as str]))

(defn- keyword->string [k] (str/capitalize (str/replace (name k) \- \ )))

(defn- show-errors
  ""
  ([error]
   (cond
     (or (nil? error) (empty? error))
     nil

     (map? error)
     [:ul {:style {:list-style :square}}
      (map (fn [[k v]] (show-errors k v)) error)]

     (sequential? error)
     [:ul {:style {:list-style :square}}
      (map show-errors error)]

     :else
     [:li {:key (str error)} (str error)]))

  ([k v]
   [:li {:key k}
    [:b (if (keyword? k) (keyword->string k) k)] ": " (show-errors v)]))

(defn- show-row-errors
  "Special-case handling for row errors - add key with row number and remove
   any entries where there is already an error for the header."
  [row-errors cols-to-exclude]
  (show-errors
   (->> row-errors
        (map #(apply (partial dissoc %) cols-to-exclude))
        (zipmap (iterate inc 2))
        (remove (fn [[_ v]] (empty? v)))
        (map (fn [[k v]] [(str "Row " k) v]))
        (into {}))))

(defn- show-sheet-errors [[sheet errors]]
  [:li {:key sheet}
   [:span "Sheet " [:b (keyword->string sheet)] ": "]
   (cond
     (sequential? errors) (show-errors errors)
     (map? errors)
     (let [header-errors (:header errors)
           row-errors (:rows errors)]
       [:div
        (show-errors header-errors)
        (show-row-errors row-errors (keys header-errors))]))])

(defn merge-dialog
  "Choose which categories of parameters to merge from `content` into
  the document, and how. `source` is the parameter set they come from,
  if they do."
  [content & {:keys [title source categories]
              :or {title "Import Parameters"}}]
  (reagent/with-let [available (filterv (parameters/categories-in content) parameters/categories)
                     *state
                     (reagent/atom
                      (merge parameters/apply-options
                             {:categories (set (or categories available))
                              :source source}))]
    (let [state @*state]
      (if (contains? content :import/errors)
        [:div.popover-dialog
         [:h2.popover-header "Failed to import spreadsheet"]
         [:button.popover-close-button
          {:on-click popover/close!}
          "⨯"]
         [:p "The spreadsheet you have chosen is not compatible with THERMOS."]
         [:b "Errors:"]
         [:div {:style {:max-height 300 :overflow-y :scroll}}
          [:ul {:style {:list-style :square}} (map show-sheet-errors (:import/errors content))]]
         
         [:div.align-right {:style {:margin-top "2em"}}
          [:button.button.button--danger
           {:on-click popover/close!
            :style {:margin-left :auto}}
           "Cancel"]]]
        
        [:div.popover-dialog
         [:h2.popover-header title]
         [:button.popover-close-button
          {:on-click popover/close!}
          "⨯"]

         [:b "Categories to import:"]
         (for [category available]
           [:div {:key category}
            [inputs/check {:label (parameters/category-labels category (name category))
                           :value (contains? (:categories state) category)
                           :on-change (fn [e]
                                        (swap!
                                         *state
                                         update :categories
                                         #((if (contains? % category) disj conj)
                                           % category)))}]])

         [:b "Existing parameters:"]
         [:div
          [inputs/check {:label "Use new parameters where names match"
                         :value (:join state)
                         :on-change #(swap! *state update :join not)}]]
         [:div
          [inputs/check {:label "Keep old parameters with different names"
                         :value (:merge state)
                         :on-change #(swap! *state update :merge not)}]]

         [:div.align-right {:style {:margin-top "2em"}}
          [:button.button.button--danger
           {:on-click popover/close!
            :style {:margin-left :auto}}
           "Cancel"]
          [:button.button
           {:on-click
            #(do (rf/dispatch [:doc/edit parameters/apply-set content @*state])
                 (popover/close!))}
           "Apply"]]]))))

(defn show-merge-dialog [content & opts]
  (popover/open!
   (into [#'merge-dialog content] opts)
   :middle))

(defn do-upload []
  (upload-file
   "/convert/excel"
   :accept "xlsx"
   :handler show-merge-dialog))

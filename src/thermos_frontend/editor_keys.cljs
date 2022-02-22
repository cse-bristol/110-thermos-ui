;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.editor-keys
  (:require [thermos-frontend.editor-state :as state]
            [thermos-frontend.operations :as operations]
            [thermos-frontend.popover :as popover]
            
            [thermos-frontend.spatial :as spatial]
            [thermos-specs.candidate :as candidate]
            [thermos-frontend.debug-box :as debug-box]

            [clojure.pprint :as pprint]

            [thermos-frontend.util :refer [target-value]]
            
            [thermos-frontend.supply-parameters :as supply-parameters]
            [thermos-frontend.candidate-editor :as candidate-editor]
            [thermos-frontend.connector-tool :as connector-tool]
            
            [goog.object :as o]
            [clojure.string :as string]))

(defn- rotate-inclusion! []
  (state/edit!
   state/state
   operations/rotate-candidates-inclusion
   (operations/selected-candidates-ids @state/state)))

(defn- edit-supply! []
  (->> @state/state
       (operations/selected-candidates)
       (filter candidate/is-building?)
       (map ::candidate/id)
       (supply-parameters/show-editor! state/state)))

(defn edit-demand-or-path! []
  (->> @state/state
       (operations/selected-candidates)
       (map ::candidate/id)
       (candidate-editor/show-editor! state/state)))

(defn- zoom-to-fit! []
  (state/edit!
   state/state
   spatial/zoom-to-selection))

(defn- select-all! []
  (state/edit! state/state operations/select-candidates
               (map ::candidate/id
                    (operations/get-filtered-candidates @state/state))
               :replace))

(defn- select-inverse! []
  (state/edit! state/state operations/select-candidates
               (map ::candidate/id
                    (operations/get-filtered-candidates @state/state))
               :xor))


(defn- show-pprint-thing []
  (popover/open!
   [:div.popover-dialog
    {:style {:max-width :80vw
             :max-height :80vh
             :display :flex}}
    [:button.popover-close-button {:on-click popover/close!} "⨯"]
    [debug-box/debug-box
     (or (seq (operations/selected-candidates @state/state))
         (dissoc @state/state :thermos-specs.document/candidates))]
    ]
   :middle))

(def descriptions
  [:table
   [:thead [:tr [:th "Key"] [:th "Function"]]]
   [:tbody
    [:tr [:th "c"] [:td "Change constraint status of selection (optional→required→forbidden)"]]
    [:tr [:th "s"] [:td "Edit supply properties for selection"]]
    [:tr [:th "z"] [:td "Zoom display to show selection"]]
    [:tr [:th "a"] [:td "Select all optional or required elements"]]
    [:tr [:th "A"] [:td "(Shift + a) Invert selection amongst optional and required elements"]]
    [:tr [:th "e"] [:td "Edit details for selected candidates"]]
    [:tr [:th "j"] [:td "Draw a connector line"]]
    [:tr [:th "g"] [:td "Select also candidates grouped with selected candidates"]]
    [:tr [:th "G"] [:td "Put all selected candidates into a group"]]
    [:tr [:th "U"] [:td "Ungroup all selected candidates"]]
    [:tr [:th "i"] [:td "Show mystic information panel"]]
    [:tr [:th "?"] [:td "Show this help"]]]]
  
  )

(defn show-help []
  (popover/open!
    [:div.popover-dialog
     [:button.popover-close-button {:on-click popover/close!} "⨯"]
     
     descriptions

     [:hr]
     [:input.text-input
      {:placeholder "Search help (type query and press return)..."
       :style {:width :100%}
       :auto-focus true
       :type :search
       :on-key-press
       #(let [key (.-key %)]
          (println key)
          (when (= key "Enter")
            (->> (target-value %)
                 (str "/help/search?q=")
                 (js/window.open))))}]]
    :middle))

(defn handle-keypress [e]
  (let [active js/document.activeElement]
    (when-not (= "INPUT" (.-nodeName active))
      (case (.-key e)
        "c" (rotate-inclusion!)
        "s" (edit-supply!)
        "z" (zoom-to-fit!)
        "a" (select-all!)
        "A" (select-inverse!)
        "e" (edit-demand-or-path!)
        "i" (show-pprint-thing)
        "j" connector-tool/toggle!
        "g" (state/fire-event! [:group-select-members])
        "G" (state/fire-event! [:group-selection])
        "U" (state/fire-event! [:ungroup-selection])
        "?" (show-help)
        :default))))




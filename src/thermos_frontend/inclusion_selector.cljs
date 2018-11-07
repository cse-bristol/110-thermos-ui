(ns thermos-frontend.inclusion-selector
  (:require [reagent.core :as reagent]
            [thermos-frontend.editor-state :as state]
            [thermos-frontend.operations :as operations]))

(declare component on-change)

;; @TODO On reflection I don't think this is the best interface for updating the selection in this way
;; It will be worth coming up with something a little more intuitive.
;; E.g. a menu of transformations you can perform on the current selection.
(defn component
  "Component for setting the `inclusion` property on the currently selected candidates."
  [document]
  [:select.select {:name "inclusion"
                   :on-change (fn [e] (on-change document e))
                   :default-value ""}
   ; Empty option to be used as placeholder
   [:option {:disabled "disabled" :hidden "hidden" :value ""}
    "Mark candidates as:"]
   [:option {:value "forbidden"} "Forbidden"]
   [:option {:value "required"} "Required"]
   [:option {:value "optional"} "Optional"]])

(defn on-change
  "Callback for change event on select element.
   Sets the selected inclusion constraint on the currently selected candidates."
  [document e]
  (let [value (.. e -target -value)
        selected-candidates-ids (operations/selected-candidates-ids @document)]
    (state/edit! document
                 operations/set-candidates-inclusion
                 selected-candidates-ids (keyword value))))

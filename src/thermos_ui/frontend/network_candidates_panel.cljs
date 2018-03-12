(ns thermos-ui.frontend.network-candidates-panel
  (:require [reagent.core :as reagent]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.frontend.virtual-table :as virtual-table]))

(declare component get-row-function)

(defn component
  "DOCSTRING"
  [document]
  (.log js/console (clj->js (first (operations/constrained-candidates @document))))
  [:div {:style {:height "100%"}}

   ;; TODO this will rerun whenever we modify anything in document we
   ;; need a cursor instead, maybe there should be some operations
   ;; things for this.
   (let [items (operations/included-candidates @document)]
     [virtual-table/component
      {:items items}
      ;; columns
      {:label "ID" :key ::candidate/id}
      {:label "Postcode" :key ::candidate/postcode}
      ])])

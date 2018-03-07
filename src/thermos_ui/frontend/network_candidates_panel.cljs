(ns thermos-ui.frontend.network-candidates-panel
  (:require [reagent.core :as reagent]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.frontend.operations :as operations]
            [thermos-ui.frontend.virtual-table :as virtual-table]))

(declare component get-row-function)

(defn component
  "DOCSTRING"
  [document]
  [:div {:style {:height "100%"}}

     ;; Attempt at virtual-table component
     [virtual-table/component
      {:columns [{:key ::candidate/id
                  :label "ID"
                  :sortable true}
                 {:key ::candidate/postcode
                  :label "Postcode"
                  :sortable true}]
       ;; TODO this needs speeding up a little, perhaps
       :items (operations/selected-candidates @document)
       :props {}}]

     ]
  )

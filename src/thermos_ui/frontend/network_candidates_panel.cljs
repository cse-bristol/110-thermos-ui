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
     [virtual-table/component
      {:columns [{:key ::candidate/selected
                  :label "Selected"
                  :sortable true}
                 {:key ::candidate/name
                  :label "Address"
                  :sortable true}
                 {:key ::candidate/postcode
                  :label "Postcode"
                  :sortable true}
                 {:key ::candidate/type
                  :label "Type"
                  :sortable true}
                 {:key ::candidate/inclusion
                  :label "Constraint"
                  :sortable true}]
       ;; TODO this needs speeding up a little, perhaps
       :items (operations/constrained-candidates @document)
       :props {}}]
     ]
  )

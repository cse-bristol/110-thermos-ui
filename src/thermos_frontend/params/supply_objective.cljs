(ns thermos-frontend.params.supply-objective
  (:require [thermos-frontend.inputs :as inputs]))

(defn- info [txt]
  [:details {:style {:display :inline-block}}
   [:summary "ℹ"]
   txt])


(defn supply-objective-parameters [doc]
  [:div
   [:div.card.flex-grow.parameters-component
    [:h1 "Accounting period"]
    [:p "Sum costs and benefits over " [inputs/number {:max 100 :min 1 :step 1}] " years. Discount future values at " [inputs/number {:max 100 :min 0 :scale 100 :step 0.1}] "% per year."]
    [:h1 "Model options"]
    [:p.has-tt
     {:title "This is the financial penalty incurred for each kWh of demand that is unmet."}
     "Curtailment cost: " [inputs/number {}] "k¤/kWh"
     
     ]
    [:p.has-tt {:title "If set, CHP engines will be allowed to produce excess heat in order to sell power."}
     [inputs/check {:label "Allow heat dumping?"}]]

    [:h1 "Emissions costs"]
    ;; Link to the other page
    [:h1 "Emissions limits"]
    ;; Link to the other page.
    [:h1 "Computing resources"]
    ;; Custom for this page
    ]]

  )

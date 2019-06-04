(ns thermos-frontend.params.buildings
  (:require [thermos-frontend.inputs :as inputs]))

(defn- building-parameter-set [title]
  [:div.card
   [:h1 title " parameters"]
   [:div
    [:h2 "Counterfactual emissions"]
    
    ]
   
   [:div
    [:h2 "Tariff"]
    [:table
     [:tbody
      [:tr
       [:td "Standing charge: "]
       [:td [inputs/number ]]
       [:td "¤/yr"]]
      [:tr
       [:td "Capacity charge: "]
       [:td [inputs/number ]]
       [:td "¤/kWp.yr"]]
      [:tr
       [:td "Unit rate: "]
       [:td [inputs/number ]]
       [:td "c/kWh"]]
      ]]]
   

   [:div
    [:h2 "Connection cost"]
    [:table
     [:tbody
      [:tr
       [:td "Fixed cost: "]
       [:td [inputs/number ]]
       [:td "¤"]]
      [:tr
       [:td "Capacity cost: "]
       [:td [inputs/number ]]
       [:td "¤/kWp"]]
      ]]]
   ]
  
  
  
  )

(defn building-parameters
  "Component for adjusting the parameters related to buildings."
  [doc]

  [:div
   [building-parameter-set "Default"]
   ]
  
  )

(ns thermos-frontend.model-parameters
  (:require [thermos-frontend.params.global :as globals]
            [thermos-frontend.params.buildings :as buildings]
            [reagent.core :as reagent]
            [clojure.string :as s]))

;; page for editing the big top-level params

;; we need to be able to set

;; loan rate & term
;; NPV rage & term
;; emissions factor
;; MIP gap
;; heat sale price (should this be per connection anyway? do we want a global default?)
;; actually let's make this / connection

(defn parameter-editor [document]
  (reagent/with-let
    [active-tab (reagent/atom :globals)

     tab (fn [key label]
           [(if (= @active-tab key) :button.selected :button)
            {:key key
             :on-click #(reset! active-tab key)}
            label])
     
     tabs (fn [stuff]
            [:div {:style {:height :100%
                           :display :flex
                           :flex-direction :column}}
             [:div.tabs
              (doall (for [[k v] stuff]
                       (tab k (s/capitalize (name k)))))]
             (stuff @active-tab)])
     ]
    [tabs {:globals [globals/parameter-editor document]
           :buildings [buildings/building-parameters document]
           :paths [:div "hi"]
           }]
    )

  )

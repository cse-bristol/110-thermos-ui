(ns thermos-frontend.model-parameters
  (:require [thermos-frontend.params.global :as globals]
            [thermos-frontend.params.tariffs :as tariffs]
            [reagent.core :as reagent]
            [clojure.string :as s]))

(defn parameter-editor [document]
  (reagent/with-let
    [active-tab (reagent/atom :tariffs)

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
           :tariffs [tariffs/tariff-parameters document]
           :paths [:div "hi"]
           }]
    ))

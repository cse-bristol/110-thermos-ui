(ns thermos-frontend.params.tariffs
  (:require [reagent.core :as reagent]
            [thermos-frontend.inputs :as inputs]
            [thermos-specs.document :as document]
            [thermos-specs.tariff :as tariff]
            [thermos-frontend.util :refer [target-value]]
            [thermos-pages.symbols :as symbols]
            [clojure.string :as string]))

(defn- vconj [v e]
  (conj (vec v) e))

(def standing-charge-unit "¤/yr")
(def capacity-charge-unit "¤/kWp.yr")
(def unit-rate-unit "c/kWh")
(def conn-fixed-unit "¤")
(def conn-var-unit "¤/kWp")

(defn- tariff-row
  "Given a reagent atom with tariffs in, and an ID of a tariff, make a table row to edit it directly"
  [*document *tariffs id]
  (let [get #(get-in @*tariffs [id %])
        put #(swap! *tariffs assoc-in [id %1] %2)
        delete-tariff #(swap! *document document/remove-tariff id)]
    [:div.card {:key id}
     [:div {:style {:display :flex }}
      [:label {:style {:flex 1}}
       "Tariff name: "
       [inputs/text
        :style {:width :50%}
        :placeholder (str "Tariff " id)
        :value (get ::tariff/name)
        :on-change #(put ::tariff/name (target-value %))]]
      [:button.button {:style {:margin-left :1em}
                       :on-click delete-tariff}
       symbols/dustbin]]
     
     [:div {:style {:display :flex}}
      [:div {:style {:display :flex :flex-direction :column :flex 1}}
       [:h1 "Annual revenues"]
       [:label
        [inputs/number
         {:title "A fixed annual payment from customers on this tariff."
          :max 1000
          :min 0
          :value (get ::tariff/standing-charge)
          :on-change #(put ::tariff/standing-charge %)
          }]
        standing-charge-unit]

       [:label
        [inputs/number
         {:title "An annual payment per kWp capacity from customers on this tariff."
          :max 1000
          :min 0
          :value (get ::tariff/capacity-charge)
          :on-change #(put ::tariff/capacity-charge %)
          }]
        capacity-charge-unit]
       [:label
        [inputs/number
         {:title "The heat price paid by customers on this tariff."
          :max 100
          :min 0
          :scale 100
          :step 0.1
          :value (get ::tariff/unit-charge)
          :on-change #(put ::tariff/unit-charge %)
          }]
        unit-rate-unit]
       ]

      [:div {:style {:display :flex :flex-direction :column :flex 1}}
       [:h1 "Connection costs"]
       [:label
        [inputs/number
         {:title "A fixed capital cost for connecting customers to this tariff."
          :max 10000
          :min -10000
          :value (get ::tariff/fixed-connection-cost)
          :on-change #(put ::tariff/fixed-connection-cost %)
          }]
        conn-fixed-unit]
       [:label
        [inputs/number
         {:title "A capital cost per kWp for connecting customers to this tariff."
          :max 1000
          :min -1000
          :value (get ::tariff/variable-connection-cost)
          :on-change #(put ::tariff/variable-connection-cost %)
          }]
        conn-var-unit]
       
       ]]
     ]))


(defn tariff-parameters
  [doc]
  (reagent/with-let
    [*tariffs (reagent/cursor doc [::document/tariffs])]
    [:div
     (doall
      (for [id (sort (keys @*tariffs))]
        (tariff-row doc *tariffs id)))
     [:button.button
      {:on-click #(swap! *tariffs
                         (fn [t]
                           (let [id (inc (reduce max -1 (keys t)))]
                             (assoc
                              t
                              id
                              {::tariff/name ""
                               ::tariff/id id ;; urgh? yes? no?
                               ::tariff/standing-charge 0
                               ::tariff/capacity-charge 0
                               ::tariff/unit-charge 0
                               
                               ::tariff/fixed-connection-cost 0
                               ::tariff/variable-connection-cost 0
                               }))))}
      symbols/plus " Add"]

     
     ]))

;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.supply-parameters
  (:require [thermos-specs.supply :as supply]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.document :as document]
            [thermos-frontend.popover :as popover]
            [thermos-frontend.inputs :as inputs]
            [thermos-frontend.params.supply-types :refer [capex-editor]]
            [re-frame.core :as rf]
            [thermos-frontend.doc :as doc]
            [reagent.core :as reagent]))

(defn- just-number-values [m] (into {} (filter (comp number? second) m)))

(defn- supply-values
  "The supply parameters a supply point has of its own"
  [candidate]
  (let [{emissions ::supply/emissions curve ::supply/capex-curve :as vals}
        (select-keys candidate (cons ::supply/technology-id supply/supply-keys))]
    (cond-> (just-number-values vals)
      (seq emissions) (assoc ::supply/emissions (just-number-values emissions))
      (seq curve)     (assoc ::supply/capex-curve curve))))

(defn- merge-supply-parameters
  "Give the candidates `candidate-ids` exactly the supply parameters in
  `parameters`; the ones it lacks they get from their supply
  technology, if they have one."
  [document candidate-ids parameters]
  (document/map-candidates
   document
   #(-> (apply dissoc % ::supply/technology-id supply/supply-keys)
        (assoc ::candidate/modified true)
        (merge parameters))
   candidate-ids))

(defn- number-row
  "A row to edit `k` in `*state`, an atom; blank means to use the value
  `inherited` from the supply type"
  [label k *state inherited & {:keys [scale step unit]}]
  [:tr
   [:td label]
   [:td [inputs/number2 {:style {:max-width :6em}
                         :min 0 :scale (or scale 1) :step step
                         :value (get @*state k)
                         :empty-value [nil (if-let [v (get inherited k)]
                                             (str (* (or scale 1) v))
                                             "")]
                         :on-change #(swap! *state (fn [s] (if (nil? %) (dissoc s k) (assoc s k %))))}]]
   [:td unit]])

(defn- supply-parameters-form [candidate-ids]
  (reagent/with-let [candidates (map (doc/value [::document/candidates]) candidate-ids)
                     ;; the values of the first candidate are what we start from
                     local-state (reagent/atom (supply-values (first candidates)))]
    (let [technologies (doc/value [::document/supply-technologies])
          state        @local-state
          tech         (get technologies (::supply/technology-id state))
          inherited    (if tech (select-keys tech supply/supply-keys) {})
          own-capex?   (some #(contains? state %) supply/capex-keys)
          put-capex    (fn [k v] (swap! local-state #(if (nil? v) (dissoc % k) (assoc % k v))))
          ]
      [:div.popover-dialog
       [:div
        [:table
         [:tbody
          [:tr
           [:td [:b "Supply type"]]
           [:td {:col-span 2}
            [inputs/select
             {:values (cons [nil "None"]
                            (for [[id t] (sort-by first technologies)] [id (::supply/name t)]))
              :value (::supply/technology-id state)
              :on-change #(swap! local-state
                                 (fn [s] (if (nil? %)
                                           (dissoc s ::supply/technology-id)
                                           (assoc s ::supply/technology-id %))))}]]]
          (when tech
            [:tr [:td {:col-span 3}
                  [:small "Leave a value blank to use the supply type's value, shown greyed out."]]])
          [:tr [:td {:col-span 3} [:b "Capacity and running costs"]]]
          [number-row "Maximum capacity" ::supply/capacity-kwp local-state inherited
           :scale (/ 1.0 1000) :step 0.1 :unit "MW"]
          [number-row "Supply cost" ::supply/cost-per-kwh local-state inherited
           :scale 100 :step 0.1 :unit "c/kWh"]
          [number-row "Annual cost" ::supply/opex-per-kwp local-state inherited
           :step 0.1 :unit "¤/kW"]
          [:tr [:td {:col-span 3} [:b "Emissions factors"]]]
          (for [e candidate/emissions-types]
            [:tr {:key e}
             [:td (name e)]
             [:td [inputs/number2
                   {:style {:max-width :6em} :min 0
                    :scale (candidate/emissions-factor-scales e)
                    :value (get-in state [::supply/emissions e])
                    :empty-value [nil (if-let [v (get-in inherited [::supply/emissions e])]
                                        (str (* (candidate/emissions-factor-scales e) v))
                                        "")]
                    :on-change #(swap! local-state
                                       (fn [s] (if (nil? %)
                                                 (update s ::supply/emissions dissoc e)
                                                 (assoc-in s [::supply/emissions e] %))))}]]
             [:td (candidate/emissions-factor-units e)]])]]
        [:b "Capital cost"]
        (when tech
          [inputs/check {:label "Use the supply type's capital cost"
                         :value (not own-capex?)
                         :on-change
                         (fn [use-type]
                           (if use-type
                             (swap! local-state #(apply dissoc % supply/capex-keys))
                             (swap! local-state merge
                                    (select-keys tech supply/capex-keys)
                                    (when-not (some #(contains? tech %) supply/capex-keys)
                                      {::supply/fixed-cost 0 ::supply/capex-per-kwp 0}))))}])
        (when (or own-capex? (not tech))
          [capex-editor (merge {::supply/capacity-kwp (::supply/capacity-kwp inherited)} state)
           put-capex])]
       [:div
        [:button.button.button--danger {:on-click #(popover/close!)}
         "Cancel"]
        [:button.button {:on-click (fn []
                                     (rf/dispatch [:doc/edit merge-supply-parameters candidate-ids @local-state])
                                     (popover/close!))}
         "OK"]]])))

(defn show-editor! [candidate-ids]
  (when (seq candidate-ids)
    (popover/open! [supply-parameters-form candidate-ids]
                   :middle)))

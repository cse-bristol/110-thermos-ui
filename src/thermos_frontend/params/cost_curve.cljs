;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.params.cost-curve
  "An editor for a piecewise linear cost curve, [[kW cost] ...]"
  (:require [thermos-frontend.inputs :as inputs]
            [thermos-pages.symbols :as symbols]))

(defn- sorted [curve] (vec (sort-by first curve)))

(defn- preview [curve]
  (let [w 160 h 80
        xs (map first curve) ys (map second curve)
        x-max (max 1 (reduce max 0 xs))
        y-max (max 1 (reduce max 0 ys))
        px #(* w (/ % x-max))
        py #(- h (* h (/ % y-max)))]
    [:svg {:width w :height h :style {:border "1px solid #ccc" :background :white}}
     (when (seq curve)
       [:polyline {:fill :none :stroke "#2a6" :stroke-width 2
                   :points (apply str (interpose " " (for [[x y] (sorted (cons [0 0] curve))]
                                                       (str (px x) "," (py y)))))}])
     (for [[i [x y]] (map-indexed vector curve)]
       [:circle {:key i :cx (px x) :cy (py y) :r 3 :fill "#2a6"}])]))

(defn cost-curve
  "`value` is the curve; `on-change` gets a new curve. A curve is the
  total capital cost of a plant of each capacity, in ¤, against its
  capacity in kW. Between the points the cost is interpolated. A
  first point at more than 0 ¤ is a fixed cost."
  [{:keys [value on-change max-kw]}]
  (let [curve (vec value)
        put (fn [i j v] (on-change (assoc-in curve [i j] (or v 0))))]
    [:div.flex-cols
     [:table
      [:thead [:tr [:th "Capacity (kW)"] [:th "Capital cost (¤)"] [:th]]]
      [:tbody
       (for [[i [kw cost]] (map-indexed vector curve)]
         [:tr {:key i}
          [:td [inputs/number2 {:style {:max-width :7em} :min 0 :value kw
                                :on-change #(put i 0 %)
                                :on-blur #(on-change (sorted curve))}]]
          [:td [inputs/number2 {:style {:max-width :8em} :min 0 :value cost
                                :on-change #(put i 1 %)}]]
          [:td [:button.button.button--small
                {:title "Remove point"
                 :on-click #(on-change (vec (concat (subvec curve 0 i) (subvec curve (inc i)))))}
                symbols/cross]]])
       [:tr
        [:td {:col-span 3}
         [:button.button.button--small
          {:on-click #(let [[x y] (peek curve)]
                        (on-change (conj curve (if x
                                                 [(+ x (or max-kw 1000)) y]
                                                 [0 0]))))}
          symbols/plus " Add point"]]]]]
     [:div {:style {:margin-left :1em}}
      [preview curve]]]))

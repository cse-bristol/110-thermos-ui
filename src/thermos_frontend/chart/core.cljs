;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.chart.core
  (:require [reagent.core :as reagent]
            [thermos-frontend.chart.context :as context
             :refer-macros [with-context with-context-value]]
            [goog.object :as o]
            [clojure.string :as string]
            [reagent.core :as r]
            [thermos-frontend.format :as format]))

(defn resizable
  "An auto-sizing wrapper around another element.
  `element` is something you want to render
  `on-size` will be called when the outer box changes size
  The rest of `atts` will be supplied as attributes to the wrapper element.
  "
  [{on-size :on-size :as atts} element]
  (reagent/with-let [*last-el (atom nil)
                     observer (js/ResizeObserver.
                               (fn [[entry]]
                                 (let [cr (o/get entry "contentRect")
                                       width  (o/get cr "width")
                                       height (o/get cr "height")
                                       el (o/get entry "target")
                                       x (o/get el "offsetLeft")
                                       y (o/get el "offsetTop")
                                       ]
                                   (when (and width height on-size)
                                     (on-size (int width) (int height) (int x) (int y))))))]
    [:div (merge
           (dissoc atts :on-size)
           {:ref (fn [el]
                   (let [last-el @*last-el
                         width  (o/get el "clientWidth")
                         height (o/get el "clientHeight")
                         x (o/get el "offsetLeft")
                         y (o/get el "offsetTop")
                         ]
                     (when last-el (.unobserve observer last-el))
                     (when el (.observe observer el))
                     (reset! *last-el el)
                     (when (and on-size width height)
                       (on-size (int width) (int height) (int x) (int y)))
                     ))})
     element]))

(defonce chart-context (context/create "chart"))

(defn- about= [[a b c d] [e f g h]]
  (and (< (Math/abs (- a e)) 5)
       (< (Math/abs (- b f)) 5)
       (< (Math/abs (- c g)) 5)
       (< (Math/abs (- d h)) 5)))

(defn +translate [x y] (str "translate(" x " " y ")"))

(defn xy-chart [{:keys [style x-range y-range]}
                & contents]
  (reagent/with-let [size (reagent/atom [100 400 0 0])]
    (let [[svg-width svg-height svg-x svg-y] @size]
      [resizable {:style style
                  :on-size (fn [& s]
                             
                             (when-not (about= s @size)
                               (reset! size s)))}
       (context/with-context [chart-context {:width svg-width
                                             :height svg-height
                                             :screen-x svg-x
                                             :screen-y svg-y
                                             :x 0
                                             :y 0}]
         `[:svg.chart ;; the chart class inverts y coordinate system
           ~{:height :100% :width :100% :view-box (str "0 0 " svg-width " " svg-height)
             :preserve-aspect-ratio "xMinYMid meet"}
           ;; the outer g is what does scaleY(-1). The inner g shifts
           ;; everything down so the bottom lines up.
           [:g [:g {:transform ~(+translate 0 (- svg-height))} ~@contents]]])])))

(defn svg-d [{:keys [width height] :as size} xys]
  (let [start (first xys)
        onward (rest xys)]
    (str "M " (* width (first start)) " " (* height (second start))
         " "
         (string/join
          " "
          (for [[x y] onward]
            (str "L " (* width x) " " (* height y)))))))

(defn line [{:keys [points line-style]}]
  (context/with-context-value [chart-context size]
    [:path (merge line-style {:d (svg-d size points)})]))

(defn stacked-bar [{:keys [series]}]
  (context/with-context-value [chart-context size]
    (let [{:keys [width height]} size

          bars
          (apply mapv vector (map :values series))
          
          bar-count (count bars)
          bar-width (/ width bar-count) ;; Add in some padding

          gap 2
          ]
      [:g
       (for [[i stack] (map-indexed vector bars)
             :let [stack (map #(Math/round (* % height)) stack)]
             [s [value base]] (map-indexed vector (map vector stack (reductions + stack)))]
         [:rect
          (merge
           (:bar-style (nth series s))
           {:key [i s]
            :x (int (+ gap (* bar-width i)))
            :y (- base value)
            :width (int (- bar-width gap gap))
            :height value})]
         
         )])))

(defn staggered-bar
  "Render a staggered bar chart into the current chart area
  `series`, an input, should be a seq of things that have :values.
  "
  [{:keys [series]}]
  (context/with-context-value [chart-context size]
    (let [{:keys [width height]} size

          longest-series (reduce max (map (comp count :values) series))
          series-count   (count series)
          
          group-gap 2
          bar-gap   1
          
          group-width (/ width longest-series)
          bar-width   (int (- (/ (- group-width group-gap) series-count)
                              bar-gap bar-gap))

          series (map-indexed vector series)
          ]
      [:g
       (for [i          (range longest-series)
             [j series] series]
         [:rect
          (merge
           (:bar-style series)
           {:key    [i j]
            :x      (int (+ group-gap
                            (* group-width i)
                            (* (+ bar-gap bar-width) j)))
            :y      0
            :width  bar-width
            :height (* height (get (:values series) i 0.0))
            })])])))

(defn clip
  "Move the chart area to a sub-region, for contents."
  [{:keys [with]} & body]
  (context/with-context-value [chart-context size]
    (let [new-size (merge size (with size))]
      (context/with-context [chart-context new-size]
        `[:g ~{:transform (+translate (- (:x new-size) (:x size))
                                      (- (:y new-size) (:y size)))}
          ~@body
          ]))))

(defn axes [{line-style :line-style axis-range :range} ]
  (let [{[y-min y-max] :y
         [x-min x-max] :x} axis-range]

    (context/with-context-value [chart-context size]
      (let [{:keys [width height]} size]
        [:g
         ;; axis lines
         [:path (merge line-style {:d (svg-d size [[0 0] [1 0]])})]
         [:path (merge line-style {:d (svg-d size [[0 0] [0 1]])})]

         ;; we want to know the scale of the range
         (when (and y-min y-max)
           (let [y-range (Math/abs (- y-max y-min))
                 log-range (int (Math/log10 y-range))
                 step-size (Math/pow 10 log-range)
                 min-step  (Math/ceil    (/ y-min step-size))
                 max-step  (Math/floor   (/ y-max step-size))
                 
                 ]
             (for [step (range min-step (inc max-step))]
               (let [value    (* step-size step)
                     fraction (/ (- value y-min) y-range)
                     ]
                 [:g {:key value :transform (+translate -10 (* fraction height))}
                  [:text
                   {:text-anchor :end :alignment-baseline :middle
                    :font-size :0.75em}
                   (format/si-number value)]
                  [:path 
                   {:key step
                    :d "M 2 0 10 0"
                    :stroke :black
                    :fill :none}
                   
                   ]]
                 )
               )
             )
           )
         ]))))

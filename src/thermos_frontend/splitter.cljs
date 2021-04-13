;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.splitter
  (:require [reagent.core :as reagent]))

(defn splitter [{:keys [left right top bottom
                        split axis on-split-change]
                 :or {axis :v}}]
  (reagent/with-let [container (atom nil)
                     dragging  (atom false)]
    (let [across (= axis :v)

          [folded split unfold-to]
          (if (vector? split)
            [true 100 (first split)]
            [false split 50])
          
          ]
      
      [:div
       {:ref #(reset! container %)
        :on-drag-over
        (fn [e] (when @dragging (.preventDefault e)))
        :on-drop
        (fn [%]
          (when @dragging
            (let [cr (.getBoundingClientRect @container)
                  cx (.-clientX %)
                  cy (.-clientY %)
                  ]
              (if across
                (on-split-change (* 100 (/ (- cx (.-x cr)) (.-width cr))))
                (on-split-change (* 100 (/ (- cy (.-y cr)) (.-height cr)))))))
          )
        :style {:display :flex
                :overflow :hidden
                :height :100%
                :width :100%
                :flex-direction (if across :row :column)}}
       [:div {:style {:flex (str split " 1 0px")
                      :overflow :hidden}}
        (when (> split 0)
          (if across left top))]


       [:div {:draggable true
              :on-drag-start #(reset! dragging true)
              :on-drag-end   #(reset! dragging false)
              :style
              (->
               {:background :#eee
                :display :flex
                :z-index 1000}
               (assoc :cursor (if across :col-resize :row-resize)
                      (if across :width :height) :8px))
              
              }
        [:button
         {:style {:width  (if across :12px :20px)
                  :height (if across :20px :12px)
                  (if across :margin-top :margin-left) :auto
                  (if across :margin-bottom :margin-right) :auto
                  (if across :margin-left :margin-top) (if folded :-4px :-2px)
                  :font-size :8px
                  :background :white
                  :border "1px #ccc solid"
                  :border-radius :2px
                  :z-index 1001
                  :padding 0
                  
                  }
          :draggable false 

          :on-click
          #(if folded
               (on-split-change unfold-to)
               (on-split-change [split])
               )}
         
         (if folded
           (if across "◀" "▲")
           (if across "▶" "▼"))
         ]
        ]

       
       [:div {:style {:overflow :hidden
                      :flex (str (- 100 split) " 1 0px")}}
        (when (< split 100)
          (if across right bottom))
        
        ]
       ]))
  )

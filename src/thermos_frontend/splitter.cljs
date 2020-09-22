(ns thermos-frontend.splitter
  (:require [reagent.core :as reagent]))

(defn splitter [{:keys [left right top bottom
                        split axis on-split-change]
                 :or {axis :v}}]
  (reagent/with-let [container (atom nil)]
    (let [across (= axis :v)

          [folded split unfold-to]
          (if (vector? split)
              [true 100 (first split)]
              [false split 50])
          
          ]
      
      [:div
       {:ref #(reset! container %)
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
              :on-drag-end
              #(let [cr (.getBoundingClientRect @container)
                     cx (.-clientX %)
                     cy (.-clientY %)
                     ]
                 
                 (println (.-left cr) cx (.-width cr))
                 (println (.-top cr) cy (.-height cr))
                 (if across
                   (on-split-change (* 100 (/ cx (.-width cr))))
                   (on-split-change (* 100 (/ cy (.-height cr))))))
              
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
                  #(do
                     (println "hey?")
                     (if folded
                      (on-split-change unfold-to)
                      (on-split-change [split])
                      ))}
         
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

(ns thermos-frontend.params.profiles
  (:require [reagent.core :as reagent]
            [clojure.string :as string]
            [thermos-pages.symbols :as syms]
            [goog.object :as o]))

(def profiles
  (reagent/atom
   {:day-types {"Winter Weekday" 30 "Winter Weekend" 7 "10 Year Peak" 1 "Summer Weekday" 50 "Summer Weekend" 20}
    :heat-profiles
    {"Residential"
     {"Winter Weekday"
      (vec (range 48))
      "Winter Weekend"
      (vec (range 48))
      }
     "Commercial" {}
     "Industrial" {}}
    :grid-offer {}

    :fuel-prices {}

    }
   )
  )

(defn- pline [xys]
  (let [start (first xys)
        onward (rest xys)
        ]
    (str "M " (first start) " " (second start)
         " "
         (string/join
          " "
          (for [[x y] onward]
            (str "L " x " " y))))))

(defn- viewable-row [{v :visible
                      on-v :on-visible
                      on-s :on-select
                      } body]
  [:div.flex-cols {:key key}
   [:div.flex-grow
    {:on-click on-s :style {:cursor :pointer}}
    body]
   [:button.button.button--link-style
    {:style {:padding 0 :line-height :inherit}
     :class (when-not v "button--dimmed")
     :on-click on-v}
    
    syms/eye]
   [:button.button.button--link-style
    {:style {:padding 0 :line-height :inherit}}
    syms/cross]])

(defn- negate-membership [s v]
  (if (contains? s v) (disj s v) (conj s v)))

(defn- resizable [{on-size :on-size :as atts} element]
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
                                     (on-size width height x y)))))]
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
                       (on-size width height x y))
                     ))})
     element]))

(defn- graphs [*profiles visible-days visible-profiles]
  (reagent/with-let [dims      (reagent/atom [200 100])
                     highlight (reagent/atom nil)
                     dragging-point (reagent/atom false)

                     to-svg-coords
                     (fn [x y]
                       
                       (let [[sw sh sx sy] @dims]
                         [(* 48.0 (/ (- x sx) sw)) (* 100 (- 1 (/ (- y sy) sh)))])
                       )
                     ]
    (let [[swidth sheight sx sy] @dims
          [width height] [48 (* 48 (/ sheight swidth))]

          tx (fn [x] x)
          ty (fn [y] (* height (- 1 (/ y 100))))
          ;; effective coordinate system is 0-48, 0-100
          
          visible-profiles @visible-profiles
          visible-days @visible-days
          profiles @*profiles
          [hi-profile hi-day hi-n] @highlight
          ]
      [resizable
       {:on-size (fn [w h x y] (reset! dims [w h x y]))
        :height :100% :width :100%}
       [:svg
        {:height :100% :width :100%
         :view-box (str "0 0 " width " " height)
         :preserve-aspect-ratio "xMinYMid meet"

         :on-mouse-down
         #(when hi-profile (reset! dragging-point true))
         :on-mouse-up
         #(do (reset! dragging-point false)
              (reset! highlight nil))
         :on-mouse-move
         #(let [[x y] (to-svg-coords (.-clientX %) (.-clientY %))]
            (when @dragging-point
              (swap! *profiles
                     assoc-in
                     [:heat-profiles hi-profile hi-day hi-n]
                     (- y 5))
              )
            
            )
         }

        (for [i (range 48)]
          [:path {:key i :d (pline [[(tx i) (ty 0.2)] [(tx i) (ty 1)]]) :fill :none :stroke :black :stroke-width 0.01}]
          )
        (for [profile-name  visible-profiles
              day-type visible-days
              :let [profile (-> profiles
                                :heat-profiles
                                (get profile-name)
                                (get day-type))
                    step-size (/ 48.0 (dec (count profile)))
                    coords (map-indexed
                            (fn [i v]
                              [(tx (* i step-size)) (ty (+ 5 v))])
                            profile)
                    
                    ]]
          (when (seq coords)
            (list
             [:path
              {:key [profile day-type]
               :d (pline coords)
               
               :fill :none
               :stroke :black
               :stroke-width 0.05
               }
              ]
             
             (for [[i [cx cy]] (map-indexed vector coords)]
               [:circle {:cx cx :cy cy :r 0.3 :key i
                         :fill
                         (if (and (= profile-name hi-profile)
                                  (= day-type hi-day)
                                  (= i hi-n))
                           :red
                           :black
                           )
                         :on-mouse-enter
                         #(reset! highlight [profile-name day-type i])
                         :on-mouse-leave
                         #(when-not @dragging-point (reset! highlight nil))
                         
                         }])))
          )
        ]])
    )
  )

(defn profiles-parameters [doc]
  (reagent/with-let [visible-days     (reagent/atom (set (keys (:day-types @profiles))))
                     visible-profiles (reagent/atom (set (keys (:heat-profiles @profiles))))]
    [:div.card.flex-rows.flex-grow {:style {:height 1}}
     [:div {:style {:padding-right :1em :overflow :auto}}
      [:div.flex-cols
       [:h1.card-header "Day types"]
       (doall
        (for [day-type (sort (keys (:day-types @profiles)))]
          [viewable-row
           {:key day-type
            :visible    (contains? @visible-days day-type)
            :on-select  #(reset! visible-days #{day-type})
            :on-visible #(swap! visible-days negate-membership day-type)}
           day-type]))
       
       [:div [:input {:type :text :placeholder "Add type"}]]]
      
      [:div.flex-cols.flex-grow
       [:div
        [:h1.card-header "Demand profiles"]
        
        (doall
         (for [profile-name (sort (keys (:heat-profiles @profiles)))]
           [viewable-row
            {:key profile-name
             :visible (contains? @visible-profiles profile-name)
             :on-select  #(reset! visible-profiles #{profile-name})
             :on-visible #(swap! visible-profiles negate-membership profile-name)
             }
            profile-name
            ]
           ))
        [:div [:input {:type :text :placeholder "Add profile"}]]
        ]
       
       [:div.flex-grow {:style {:border "1px #eee solid"}}
        [graphs profiles visible-days visible-profiles]
        ]]]
     
     ])
  
  )

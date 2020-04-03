(ns thermos-frontend.params.profiles
  (:require [reagent.core :as reagent]
            [clojure.string :as string]
            [thermos-pages.symbols :as syms]
            [thermos-frontend.inputs :as inputs]
            [thermos-specs.candidate :as candidate]
            [goog.object :as o]
            [clojure.pprint]
            [thermos-util :refer-macros [forM]]))

(defn- add-day-type [profiles day-type]
  (-> profiles
      (assoc-in [:day-types day-type]
                {:frequency 1 :divisions 24})))

(defn- add-profile [profiles profile]
  (-> profiles
      (assoc-in [:heat-profiles profile]
                (forM [[day-type {d :divisions}] (:day-types profiles)]
                  day-type (vec (repeat d 0.0))))))

(defn- add-fuel [profiles fuel]
  (let [new-fuel (forM [[day-type {d :divisions}] (:day-types profiles)
                        :let [z (vec (repeat d 0.0))]]
                   day-type
                   (merge {:price z}
                          (forM [e candidate/emissions-types] e z)))]
    (println new-fuel)
    (assoc-in profiles [:fuel fuel] new-fuel)))

(defn- add-tab-button [{:keys [placeholder on-add-tab]
                        :or {placeholder "Add"}}]
  (reagent/with-let [active (reagent/atom false)]
    [:li.tabs__tab
     {:on-click #(reset! active true)}
     (if @active
       [:input.input
        {:placeholder placeholder
         :ref #(and % (.focus %))
         :on-blur #(reset! active false)
         :on-key-up
         #(when (= (.-key %) "Enter")
            (and on-add-tab (on-add-tab (-> % .-target .-value)))
            (reset! active false))
         }]
       "+")]))

(defn- vector-row [{divisions :divisions values :values on-change :on-change :as atts} & prefix]
  `[:tr ~(dissoc atts :divisions :values :on-change)
    ~@prefix
    ~@(doall
       (for [i (range divisions)]
         [:td {:key i :style {:margin 0 :padding 0 :border "1px grey solid"}}
          [:input {:type :text
                   :pattern "^\\d(\\.\\d\\d?)?$"
                   :style {:width :1.8em :margin 0 :padding 0 :border :none}
                   :default-value (get values i)
                   :placeholder   (get values i)
                   :on-change #(-> % .-target .-value
                                   (js/parseFloat)
                                   (->> (on-change i)))}]]))])

(defn- put-vec
  "Put x into vector v at index i, extending v if needs be and padding with zeroes."
  [v i x]
  (let [v (vec v)]
    (assoc
     (if (<= i (count v))
       v
       (vec (take i (concat v (repeat 0)))))
     i x)))

(defn- day-type-editor [profiles day-type]
  (reagent/with-let [heat-profile-names   (reagent/track #(sort (keys (:heat-profiles @profiles))))
                     fuel-names           (reagent/track #(sort (keys (:fuel @profiles))))]
    
    (let [*divisions (reagent/cursor profiles [:day-types day-type :divisions])
          *frequency (reagent/cursor profiles [:day-types day-type :frequency])

          divisions @*divisions
          heat-profile-names @heat-profile-names
          fuel-names @fuel-names

          grid-offer (reagent/cursor profiles [:grid-offer day-type])
          
          heat-profile-values
          (forM [hpn heat-profile-names]
            hpn
            (reagent/cursor profiles [:heat-profiles hpn day-type]))
          

          fuel-price-values
          (forM [fnam fuel-names]
            fnam
            (reagent/cursor profiles [:fuel fnam day-type :price]))

          fuel-emissions-values
          (forM [e candidate/emissions-types]
            e
            (forM [fnam fuel-names]
              fnam
              (reagent/cursor profiles [:fuel fnam day-type e])))
          
          ]

      [:div {:style {:overflow :auto}}
       [:label "Relative frequency: "
        [inputs/number {:value-atom *frequency
                        :key day-type ;; this is on here to make it
                                      ;; re-render. Otherwise reagent sees it as same element
                        :max 365
                        :min 0
                        :step 1}]]
       
       [:label {:style {:margin-left :1em}}
        "Time precision: "
        [inputs/number {:value @*divisions
                        :key day-type
                        :on-change
                        (fn [x]
                          ;; this is messy, we need to downsample or upsample
                          ;; everything in this day type
                          )
                        :max 48
                        :step 1
                        :min 1}]]

       [:div {:style {:height :1em}}]
       
       [:table {:style {:border-spacing 0 :border-collapse :collapse }}
        [:thead [:tr [:th [:em "Heat demand"]] (for [i (range divisions)] [:th {:key i} (str i)])]]
        [:tbody {:key day-type}
         (doall
          (for [hpn heat-profile-names
                :let [values (get heat-profile-values hpn)]]
            [vector-row {:key hpn
                         :divisions divisions
                         :values    @values
                         :on-change (fn [i v] (swap! values put-vec i v))}
             [:td hpn]]))
         [:tr [:td {:padding :1em}
               [add-tab-button {:placeholder "Add profile"
                                :on-add-tab
                                #(swap! profiles add-profile %)
                                }]
               ]]
         [:tr [:th [:em "Fuel price"]] (for [i (range divisions)] [:th {:key i} (str i)])]
         [vector-row {:divisions divisions :values @grid-offer
                      :key day-type
                      :on-change (fn [i v] (swap! grid-offer put-vec i v))}
          [:td  "Grid offer"]]

         (doall
          (for [fnam fuel-names :let [values (get fuel-price-values fnam)]]
            [vector-row {:values @values :divisions divisions
                         :key fnam
                         :on-change (fn [i v] (swap! values put-vec i v))}
             [:td fnam]]))

         [:tr [:td {:padding :1em}
               [add-tab-button {:placeholder "Add fuel"
                                :on-add-tab #(swap! profiles add-fuel %)
                                }]]]
         (doall
          (for [e candidate/emissions-types]
            (list
             [:tr {:key :header} [:td {:padding :1em} " "]]
             [:tr {:key [e 1]}
              [:th [:em (name e)]]
              (for [i (range divisions)] [:th {:key i} (str i)])]
             (doall
              (for [fnam fuel-names :let [values (get-in fuel-emissions-values [e fnam])]]
                [vector-row {:values @values :divisions divisions
                             :key fnam
                             :on-change (fn [i v] (swap! values put-vec i v))}
                 [:td fnam]]))
             )))
         ]]]
      )))



(defn profiles-parameters [doc]
  (reagent/with-let [profiles (reagent/cursor doc [:supply/profiles])
                     day-types     (reagent/cursor profiles [:day-types])
                     selected-day (reagent/atom (first (keys @day-types)))]
    (let [day-type @selected-day]
      [:div.card.flex-grow
       [:ul.tabs__tabs.tabs__tabs--pills
        (for [a-day-type (map first
                              (sort-by (comp - :frequency second)
                                       @day-types))]
          [:li.tabs__tab {:key a-day-type
                          :on-click #(reset! selected-day a-day-type)
                          :class (when (= day-type a-day-type) "tabs__tab--active")}
           a-day-type]

          
          )
        [add-tab-button {:placeholder "New day type"
                         :on-add-tab
                         (fn [day-type]
                           (swap! profiles add-day-type day-type)
                           (reset! selected-day day-type))}]]
       
       [day-type-editor profiles day-type]

       
       ])))


;; (defn- pline [xys]
;;   (let [start (first xys)
;;         onward (rest xys)
;;         ]
;;     (str "M " (first start) " " (second start)
;;          " "
;;          (string/join
;;           " "
;;           (for [[x y] onward]
;;             (str "L " x " " y))))))

;; (defn- viewable-row [{v :visible
;;                       on-v :on-visible
;;                       on-s :on-select
;;                       } body]
;;   [:div.flex-cols {:key key}
;;    [:div.flex-grow
;;     {:on-click on-s :style {:cursor :pointer}}
;;     body]
;;    [:button.button.button--link-style
;;     {:style {:padding 0 :line-height :inherit}
;;      :class (when-not v "button--dimmed")
;;      :on-click on-v}
    
;;     syms/eye]
;;    [:button.button.button--link-style
;;     {:style {:padding 0 :line-height :inherit}}
;;     syms/cross]])

;; (defn- negate-membership [s v]
;;   (if (contains? s v) (disj s v) (conj s v)))

;; (defn- resizable [{on-size :on-size :as atts} element]
;;   (reagent/with-let [*last-el (atom nil)
;;                      observer (js/ResizeObserver.
;;                                (fn [[entry]]
;;                                  (let [cr (o/get entry "contentRect")
;;                                        width  (o/get cr "width")
;;                                        height (o/get cr "height")
;;                                        el (o/get entry "target")
;;                                        x (o/get el "offsetLeft")
;;                                        y (o/get el "offsetTop")
;;                                        ]
;;                                    (when (and width height on-size)
;;                                      (on-size width height x y)))))]
;;     [:div (merge
;;            (dissoc atts :on-size)
;;            {:ref (fn [el]
;;                    (let [last-el @*last-el
;;                          width  (o/get el "clientWidth")
;;                          height (o/get el "clientHeight")
;;                          x (o/get el "offsetLeft")
;;                          y (o/get el "offsetTop")
;;                          ]
;;                      (when last-el (.unobserve observer last-el))
;;                      (when el (.observe observer el))
;;                      (reset! *last-el el)
;;                      (when (and on-size width height)
;;                        (on-size width height x y))
;;                      ))})
;;      element]))

;; (defn- graphs [*profiles visible-days visible-profiles]
;;   (reagent/with-let [dims      (reagent/atom [200 100])
;;                      highlight (reagent/atom nil)
;;                      dragging-point (reagent/atom false)

;;                      to-svg-coords
;;                      (fn [x y]
                       
;;                        (let [[sw sh sx sy] @dims]
;;                          [(* 48.0 (/ (- x sx) sw)) (* 100 (- 1 (/ (- y sy) sh)))])
;;                        )
;;                      ]
;;     (let [[swidth sheight sx sy] @dims
;;           [width height] [48 (* 48 (/ sheight swidth))]

;;           tx (fn [x] x)
;;           ty (fn [y] (* height (- 1 (/ y 100))))
;;           ;; effective coordinate system is 0-48, 0-100
          
;;           visible-profiles @visible-profiles
;;           visible-days @visible-days
;;           profiles @*profiles
;;           [hi-profile hi-day hi-n] @highlight
;;           ]
;;       [resizable
;;        {:on-size (fn [w h x y] (reset! dims [w h x y]))
;;         :height :100% :width :100%}
;;        [:svg
;;         {:height :100% :width :100%
;;          :view-box (str "0 0 " width " " height)
;;          :preserve-aspect-ratio "xMinYMid meet"

;;          :on-mouse-down
;;          #(when hi-profile (reset! dragging-point true))
;;          :on-mouse-up
;;          #(do (reset! dragging-point false)
;;               (reset! highlight nil))
;;          :on-mouse-move
;;          #(let [[x y] (to-svg-coords (.-clientX %) (.-clientY %))]
;;             (when @dragging-point
;;               (swap! *profiles
;;                      assoc-in
;;                      [:heat-profiles hi-profile hi-day hi-n]
;;                      (- y 5))
;;               )
            
;;             )
;;          }

;;         (for [i (range 48)]
;;           [:path {:key i :d (pline [[(tx i) (ty 0.2)] [(tx i) (ty 1)]]) :fill :none :stroke :black :stroke-width 0.01}]
;;           )
;;         (for [profile-name  visible-profiles
;;               day-type visible-days
;;               :let [profile (-> profiles
;;                                 :heat-profiles
;;                                 (get profile-name)
;;                                 (get day-type))
;;                     step-size (/ 48.0 (dec (count profile)))
;;                     coords (map-indexed
;;                             (fn [i v]
;;                               [(tx (* i step-size)) (ty (+ 5 v))])
;;                             profile)
                    
;;                     ]]
;;           (when (seq coords)
;;             (list
;;              [:path
;;               {:key [profile day-type]
;;                :d (pline coords)
               
;;                :fill :none
;;                :stroke :black
;;                :stroke-width 0.05
;;                }
;;               ]
             
;;              (for [[i [cx cy]] (map-indexed vector coords)]
;;                [:circle {:cx cx :cy cy :r 0.3 :key i
;;                          :fill
;;                          (if (and (= profile-name hi-profile)
;;                                   (= day-type hi-day)
;;                                   (= i hi-n))
;;                            :red
;;                            :black
;;                            )
;;                          :on-mouse-enter
;;                          #(reset! highlight [profile-name day-type i])
;;                          :on-mouse-leave
;;                          #(when-not @dragging-point (reset! highlight nil))
                         
;;                          }])))
;;           )
;;         ]])
;;     )
;;   )

;; (defn profiles-parameters [doc]
;;   (reagent/with-let [visible-days     (reagent/atom (set (keys (:day-types @profiles))))
;;                      visible-profiles (reagent/atom (set (keys (:heat-profiles @profiles))))]
;;     [:div.card.flex-rows.flex-grow {:style {:height 1}}
;;      [:div {:style {:padding-right :1em :overflow :auto}}
;;       [:div.flex-cols
;;        [:h1.card-header "Day types"]
;;        (doall
;;         (for [day-type (sort (keys (:day-types @profiles)))]
;;           [viewable-row
;;            {:key day-type
;;             :visible    (contains? @visible-days day-type)
;;             :on-select  #(reset! visible-days #{day-type})
;;             :on-visible #(swap! visible-days negate-membership day-type)}
;;            day-type]))
       
;;        [:div [:input {:type :text :placeholder "Add type"}]]]
      
;;       [:div.flex-cols.flex-grow
;;        [:div
;;         [:h1.card-header "Demand profiles"]
        
;;         (doall
;;          (for [profile-name (sort (keys (:heat-profiles @profiles)))]
;;            [viewable-row
;;             {:key profile-name
;;              :visible (contains? @visible-profiles profile-name)
;;              :on-select  #(reset! visible-profiles #{profile-name})
;;              :on-visible #(swap! visible-profiles negate-membership profile-name)
;;              }
;;             profile-name
;;             ]
;;            ))
;;         [:div [:input {:type :text :placeholder "Add profile"}]]
;;         ]
       
;;        [:div.flex-grow {:style {:border "1px #eee solid"}}
;;         (reagent/with-let [graph-or-grid (reagent/atom :graph)]
;;           (case @graph-or-grid
;;             :graph [graphs profiles visible-days visible-profiles]
;;             :grid  []
;;             ))
;;         ]]]
     
;;      ])
  
;;   )

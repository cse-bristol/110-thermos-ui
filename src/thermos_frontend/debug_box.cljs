(ns thermos-frontend.debug-box)

(defn- debug-box- [obj]
  (cond
    (map? obj)
    [:table {:style {:border "2px grey dotted" :margin :2px}}
     [:tbody
      (for [k (sort (keys obj))
            :let [v (get obj k)]]
        [:tr {:key k}
         [:td [:b (debug-box- k)]]
         [:td (debug-box- v)]])]]

    (vector? obj)
    [:div {:style {:margin-left :4px}}
     (for [[i o] (map-indexed vector obj)]
       [:div {:key i :style {:border "1px black solid"}} (debug-box- o)])]

    (set? obj)
    [:div {:style {:margin-left :4px :border "1px orange solid"}}
     (for [[i o] (map-indexed vector obj)]
       [:div {:key i} (debug-box- o)])]
    
    (keyword? obj)
    [:span {:style {:display :inline-block :white-space :nowrap}}
     (str obj)
     ]
    
    (string? obj)
    obj
    (seqable? obj)
    [:div {:style {:margin-left :4px :border "1px green dashed"}}
     
     (for [[i o] (map-indexed vector obj)]
       [:div {:key i} (debug-box- o)])
     ]

    :default
    (str obj)))

(defn debug-box [obj]
  [:div {:style {:overflow-y :auto :max-height :50vh}}
   (debug-box- obj)])

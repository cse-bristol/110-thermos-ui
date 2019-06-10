(ns thermos-frontend.debug-box
  (:require [reagent.core :as reagent]
            [clojure.string :as string]))

(defn- debug-box- [pattern obj]
  (cond
    (map? obj)
    [:table {:style {:border "2px grey dotted" :margin :2px}}
     [:tbody
      (for [k (sort (keys obj))
            :when (re-find pattern (str k))
            :let [v (get obj k)]]
        [:tr {:key (str k)}
         [:td [:b (debug-box- pattern k)]]
         [:td (debug-box- pattern v)]])]]

    (vector? obj)
    [:div {:style {:margin-left :4px}}
     (for [[i o] (map-indexed vector obj)]
       [:div {:key i :style {:border "1px black solid"}} (debug-box- pattern o)])]

    (set? obj)
    [:div {:style {:margin-left :4px :border "1px orange solid"}}
     (for [[i o] (map-indexed vector obj)]
       [:div {:key i} (debug-box- pattern o)])]
    
    (keyword? obj)
    [:span {:style {:display :inline-block :white-space :nowrap}}
     (str obj)]
    
    (string? obj)
    obj
    
    (seqable? obj)
    [:div {:style {:margin-left :4px :border "1px green dashed"}}
     
     (for [[i o] (map-indexed vector obj)]
       [:div {:key i} (debug-box- pattern o)])
     ]

    :default
    (str obj)))

(defn debug-box [obj]
  (reagent/with-let [search (reagent/atom "")]
    [:div {:style {:overflow-y :auto :max-height :50vh}}
     [:input {:value @search :on-change #(reset! search (.. % -target -value))}]
     (debug-box-
      (try (re-pattern @search)
           (catch js/Error e (re-pattern "")))
      obj)]))

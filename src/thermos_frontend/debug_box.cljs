(ns thermos-frontend.debug-box
  (:require [reagent.core :as reagent]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [clojure.pprint :as pprint]))

(defn- debug-box- [obj]
  (cond
    (map? obj)
    [:table {:style {:border "2px grey dotted" :margin :2px}}
     [:tbody
      (for [k (sort (keys obj))
            :let [v (get obj k)]]
        [:tr {:key (str k)}
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
     (str obj)]
    
    (string? obj)
    obj
    
    (seqable? obj)
    [:div {:style {:margin-left :4px :border "1px green dashed"}}
     
     (for [[i o] (map-indexed vector obj)]
       [:div {:key i} (debug-box- o)])
     ]

    :default
    (str obj)))

(defn mapt [f c]
  (cond
    (vector? c)
    (mapv f c)

    (set? c)
    (into #{} (map f c))

    :else
    (map f c)))

(defn filter-pattern [p o]
  (cond
    (map? o)
    (into {}
          (for [[k v] o
                :let [km (re-find p (str k))
                      v' (if km v
                             (filter-pattern p v))
                      ]
                :when (or km (and (coll? v') (not-empty v')))]
            [k v']))

    (vector? o)
    (vec (keep (partial filter-pattern p) o))

    (set? o)
    (set (keep (partial filter-pattern p) o))

    (coll? o)
    (keep (partial filter-pattern p) o)))

(defn debug-box [obj]
  (reagent/with-let [search (reagent/atom "")]
    [:div
     [:input {:value @search :on-change #(reset! search (.. % -target -value))}]
     [:div {:style {:overflow-y :auto :max-height :75vh}}
      (debug-box-
       (let [pattern @search]
         (if (= "" pattern) obj
             (try (filter-pattern (re-pattern @search) obj)
                  (catch js/Error e obj)))))
      ]]))

(defn pprint-pre [obj]
  [:pre {:style {:white-space :pre-wrap}}
   (with-out-str (pprint/pprint obj))])

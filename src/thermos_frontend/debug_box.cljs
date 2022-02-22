;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.debug-box
  (:require [reagent.core :as reagent]
            [clojure.string :as string]
            [clojure.pprint :as pprint]))

(declare data-view)

(defn- tabular? [v]
  (and (or (seq? v)
           (vector? v))
       (let [ks (atom :none)]
         (every?
          (fn [x] (and (map? x)
                       (let [ksx (set (keys x))]
                         (if (= :none @ks)
                           (do (reset! ks ksx) true)
                           (= @ks ksx)))))
          v))))

(defn- table-view [vs]
  (let [headers (-> (set (keys (first vs)))
                    (disj ::id)
                    (->> (sort-by str)))]
    [:table
     [:thead [:tr
              [:th
               {:style {:cursor :pointer}
                :on-click #(js/navigator.clipboard.writeText
                            (with-out-str (pprint/pprint vs)))}
               "ID"]
              (for [k headers] [:th {:key k} (str k)])]]
     [:tbody
      (for [[n r] (map-indexed vector vs)
            :let [rid (or (::id r n))]]
        [:tr
         [:td {:key ::id
               :style {:cursor :pointer}
               :on-click #(js/navigator.clipboard.writeText
                           (with-out-str (pprint/pprint r)))}
          (str rid)]
         (for [k headers]
           [:td {:key k} [data-view (get r k)]])])]]))

(defn- map-table-view [m]
  [table-view (for [[k v] m] (assoc v ::id k))])

(defn- map-view [{:keys [initial-open]} d]
  (reagent/with-let [open (reagent/atom initial-open)]
    (let [is-open @open]
      [:details {:open is-open}
       [:summary {:on-click #(swap! open not)}
        (binding [*print-length* 1] (pr-str d))]
       (when is-open
         [:table {:style {:margin-left :1em}}
          [:tbody
           (for [[k v] d]
             [:tr {:key k} [:td [data-view k]] [:td [data-view v]]])]])])))

(defn- sequence-view [d]
  (reagent/with-let [open (reagent/atom false)]
    (let [is-open @open]
      [:details {:open is-open}
       [:summary {:on-click #(swap! open not)}
        (binding [*print-length* 1] (pr-str d))]
       
       (when is-open
         [:div {:style {:margin-left :1em}}
          [:table {:style {:margin-left :1em}}
           [:tbody
            (for [[k v] (map-indexed vector d)]
              [:tr {:key k} [:td [data-view k]] [:td [data-view v]]])]]
          
          ])
       ]))
  )

(defn data-view
  ([d] [data-view {} d])
  ([{:keys [open]} d]
   (cond
     (tabular? d)
     (table-view d)

     (and (map? d) (tabular? (vals d)))
     (map-table-view d)
     
     (map? d)
     (map-view {:initial-open open} d)

     (or (vector? d) (list? d) (set? d))
     (sequence-view d)

     :else
     (let [s (pr-str d)]
       [:span {:style {:cursor :pointer
                       :color
                       (cond (string? d) "orange"
                             (number? d) "darkcyan"
                             (keyword? d) "darkgreen"
                             :else "black"
                             
                             )
                       }
               :on-click #(js/navigator.clipboard.writeText
                           (if (string? d) d s))
               }
        (if (or (and (string? d)
                     (> (.-length s) 33))
                (> (.-length s) 100))
          (str (.substring s 0 35) "...")
          s)
        ]))))

(defn debug-box [obj]
  [:div {:style {:overflow-x :auto :overflow-y :auto}}
   [data-view {:open true} obj]])


(defn pprint-pre [obj]
  [:pre {:style {:white-space :pre-wrap}}
   (with-out-str (pprint/pprint obj))])

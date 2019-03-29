(ns thermos-pages.menu
  (:require [thermos-pages.common :as common]
            [clojure.string :as string])
  #?(:cljs (:require-macros [thermos-pages.common :as common]))
  )

(defn menu [& items]
  [:span.menu {:style {:margin-left :auto}}
   [:button {:style {:vertical-align :middle}}
    [:img {:style {:vertical-align :middle}
           :src "/favicon.ico" :width "16"}]
    [:span {:style {:color "#ddd"}} " ▼"]]
   
   [:div
    (for [item items]
      [:div item])]])


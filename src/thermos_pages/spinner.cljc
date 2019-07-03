(ns thermos-pages.spinner
  (:require [thermos-frontend.theme :refer [icon]]))

(defn spinner
  ([] (spinner {}))
  ([{:keys [size]}]
   [:div.spin-around 
    (merge (when size {:width (str size "px") :height (str size "px")})
           {:style {:display :inline-block}})
    icon]))


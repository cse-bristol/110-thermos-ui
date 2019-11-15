(ns thermos-pages.spinner
  (:require [thermos-frontend.theme :refer [icon]]))

(defn spinner
  ([] (spinner {}))
  ([{:keys [size]}]
   [:div.spin-around
    {:style (merge {:display :inline-block}
                   (when size {:width (str size "px") :height (str size "px")}))}
    (cond-> icon
      size (update 1 assoc :width size :height size))]))




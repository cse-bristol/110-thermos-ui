(ns thermos-pages.spinner)

(defn spinner [& {:keys [size]}]
  (if size
    [:div {:style {:width (str size "px")
                   :height (str size "px")
                   :transform (str "scale(" (/ size 64.0) ")")}}
     (spinner)]
    [:div.spinner (repeat 12 [:div])]))




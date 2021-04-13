;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

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




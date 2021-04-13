;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.chart.context
  (:require [reagent.core :as reagent]))

(defn create [n] #?(:cljs (js/React.createContext n)))

(defmacro with-context [[context# value#] form#]
  `[:> (.-Provider ~context#) {:value ~value#} ~form#])

(defmacro with-context-value [[context# value#] form#]
  `[:> (.-Consumer ~context#)
    (fn [~value#]
      (let [~value# (cljs.core/js->clj ~value# :keywordize-keys true)]
        (reagent/as-element ~form#)))])

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

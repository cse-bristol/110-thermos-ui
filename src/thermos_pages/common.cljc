;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-pages.common
  (:require [clojure.string :as string])
  #?(:clj (:require [net.cgrand.macrovich :as macros]
                    [clojure.walk :as walk]
                    [com.rpl.specter :refer :all]))
  #?(:cljs (:require-macros [net.cgrand.macrovich :as macros])))

(defn style* [m]
  #?(:cljs m
     :clj (when (map? m)
            (str
             (string/join
              ";"
              (for [[k v] m]
                (str
                 (if (keyword? k) (name k) (str k)) ":"
                 (if (keyword? v) (name v) (str v)))))))))

(defn style [& {:keys [] :as m}]
  (style* m))

(defmacro fn-js "Same as (fn ...) but only when targetting cljs"
  {:style/indent :defn}
  [& args]
  (macros/case :cljs
    `(fn ~@args)))


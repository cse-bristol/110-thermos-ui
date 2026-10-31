;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-pages.common
  (:require [clojure.string :as string]))

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

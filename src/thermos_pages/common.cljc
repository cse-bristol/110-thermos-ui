(ns thermos-pages.common
  (:require [clojure.string :as string]))

(defn style [& {:keys [] :as m}]
  #?(:cljs m
     :clj (when (seq m)
            (str
             (string/join
              ";"
              (for [[k v] m]
                (str
                 (if (keyword k) (name k) (str k)) ":"
                 (if (keyword v) (name v) (str v)))))))))

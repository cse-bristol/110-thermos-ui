;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-test.specs.util
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            ))

;; TODO make this print the explanation with newlines in it
(defmethod assert-expr 'valid? [msg form]
  (let [args (rest form)]
    `(let [result# (s/valid? ~@args)]
       (if result#
         (do-report {:type :pass :message ~msg
                     :expected '~form :actual '~form})
         (do-report {:type :fail :message ~msg
                     :expected '~form :actual (first
                                               (::s/problems (s/explain-data ~@args)))})))))

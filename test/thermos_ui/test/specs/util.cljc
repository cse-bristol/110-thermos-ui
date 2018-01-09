(ns thermos-ui.test.specs.util
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            ))

(defmethod assert-expr 'conforms-to? [msg form]
  (let [args (rest form)]
    `(let [result# (s/valid? ~@args)]
       (if result#
         (do-report {:type :pass :message ~msg
                     :expected '~form :actual '~form})
         (do-report {:type :fail :message ~msg
                     :expected '~form :actual (s/explain-str ~@args)})))))

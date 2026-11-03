;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.spreadsheet.supply-types-test
  (:require [thermos-backend.spreadsheet.core :as spreadsheet]
            [thermos-backend.spreadsheet.common :as common]
            [thermos-specs.defaults :as defaults]
            [thermos-specs.document :as document]
            [thermos-specs.supply :as supply]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [clojure.test :as t]))

(t/deftest curve-cells
  (t/is (= "0:10 500:200.5" (common/supply-curve->string [[0 10] [500 200.5]])))
  (t/is (= [[0.0 10.0] [500.0 200.0]] (common/string->supply-curve "500:200, 0:10")))
  (t/is (nil? (common/string->supply-curve "")))
  (t/is (nil? (common/string->supply-curve "0:10 oops"))))

(t/deftest supply-types-round-trip
  (let [f (java.io.File/createTempFile "supply-types" ".xlsx")]
    (try
      (with-open [out (io/output-stream f)]
        (common/write-to-stream (spreadsheet/to-spreadsheet defaults/default-document) out))
      (let [doc (spreadsheet/from-spreadsheet f)
            ;; numbers come back as doubles, and missing emissions as 0
            tidy (fn [t] (-> (walk/postwalk #(if (number? %) (double %) %) t)
                             (dissoc ::supply/id)
                             (update ::supply/emissions #(into {} (remove (comp zero? val)) %))))
            by-name #(into {} (for [t (vals (::document/supply-technologies %))]
                                [(::supply/name t) (tidy t)]))]
        (t/is (nil? (:import/errors doc)) (pr-str (:import/errors doc)))
        (t/is (= (by-name defaults/default-document) (by-name doc))))
      (finally (.delete f)))))

;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.spreadsheet.output-test
  "The spreadsheet's cells for the documents in thermos-backend.test-documents
  are the same as they were before the tables it is made from were shared
  with the GeoPackage (golden_output.edn)"
  (:require [thermos-backend.spreadsheet.core :as spreadsheet]
            [thermos-backend.spreadsheet.common :as common]
            [thermos-backend.test-documents :as test-documents]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :as t]))

(def golden
  (delay (edn/read-string (slurp (io/resource "thermos_backend/spreadsheet/golden_output.edn")))))

(defn- cells [doc]
  (mapv (fn [tab] [(:name tab) (common/make-rows tab)])
        (spreadsheet/to-spreadsheet doc)))

(t/deftest spreadsheet-unchanged
  (t/is (= (:unsolved @golden) (cells test-documents/unsolved-document)))
  (t/is (= (:solved @golden) (cells test-documents/solved-document))))

(t/deftest tables-are-the-other-tabs
  (let [candidate-tabs #{"Buildings" "Networked buildings" "Other buildings"
                         "Supply points" "Paths & pipes"}]
    (doseq [doc [test-documents/unsolved-document test-documents/solved-document]]
      (t/is (= (remove candidate-tabs (map :name (spreadsheet/to-spreadsheet doc)))
               (map :name (spreadsheet/to-tables doc)))))))

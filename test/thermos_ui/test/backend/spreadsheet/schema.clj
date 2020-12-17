(ns thermos-test.backend.spreadsheet.schema
  (:require [clojure.test :refer :all]
            [thermos-backend.spreadsheet.schema :as schema]
            [thermos-specs.defaults :as defaults]
            [thermos-backend.spreadsheet.core :as ss-core]
            [thermos-backend.spreadsheet.common :as ss-common]
            [clojure.java.io :as io]))

(def default-sheet
  (let [out-sheet (ss-core/to-spreadsheet defaults/default-document)
        baos (java.io.ByteArrayOutputStream.)]
    (ss-common/write-to-stream out-sheet baos)
    (ss-common/read-to-tables (io/input-stream (.toByteArray baos)))))

(deftest schema-coercion-works
  (let [tarriff-with-string-unit-charge 
        (assoc-in default-sheet [:thermos-specs.document/tariffs 1 :unit-charge] "0.05"),
        
        tarriff-with-int-unit-charge
        (assoc-in default-sheet [:thermos-specs.document/tariffs 1 :unit-charge] 5),]
    
    (is (nil? (:import/errors (schema/validate-network-model-ss default-sheet))))
    (is (nil? (:import/errors (schema/validate-network-model-ss tarriff-with-string-unit-charge))))
    (is (nil? (:import/errors (schema/validate-network-model-ss tarriff-with-int-unit-charge))))))

(deftest validation-error-reporting
  (let [no-tarriffs-sheet (dissoc default-sheet :tariffs)
        no-unit-rate (assoc-in default-sheet [:tariffs :rows] 
                               '({:tariff-name "Standard", 
                                  :capacity-charge 0.0, 
                                  :standing-charge 50.0, 
                                  :spreadsheet/row 0}))
        bad-unit-rate (assoc-in default-sheet [:tariffs :rows] 
                                '({:tariff-name "Standard", 
                                   :unit-rate "bad", 
                                   :capacity-charge 0.0, 
                                   :standing-charge 50.0, 
                                   :spreadsheet/row 0}))]
    (is (= {:tariffs ["missing required key"]}
           (:import/errors (schema/validate-network-model-ss no-tarriffs-sheet))))
    (is (= {:tariffs {:rows [{:unit-rate ["missing required key"]}]}}
           (:import/errors (schema/validate-network-model-ss no-unit-rate))))
    (is (= {:tariffs {:rows [{:unit-rate ["should be a double"]}]}}
           (:import/errors (schema/validate-network-model-ss bad-unit-rate))))))

(deftest pipe-cost-validation
  (let [no-pipe-costs-sheet (dissoc default-sheet :pipe-costs)
        cols-need-coercing (-> default-sheet
                               (assoc-in [:pipe-costs :rows] (vec (get-in default-sheet [:pipe-costs :rows])))
                               (assoc-in [:pipe-costs :rows 0 :nb] "20")
                               (assoc-in [:pipe-costs :rows 0 :soft] "206"))
        bad-fixed-col (-> default-sheet
                          (assoc-in [:pipe-costs :rows] (vec (get-in default-sheet [:pipe-costs :rows])))
                          (assoc-in [:pipe-costs :rows 0 :nb] "aaa")
                          (assoc-in [:pipe-costs :rows 0 :soft] "206"))
        bad-variable-col (-> default-sheet
                             (assoc-in [:pipe-costs :rows] (vec (get-in default-sheet [:pipe-costs :rows])))
                             (assoc-in [:pipe-costs :rows 0 :nb] "20")
                             (assoc-in [:pipe-costs :rows 0 :soft] "aaa"))
        bad-both-col (-> default-sheet
                         (assoc-in [:pipe-costs :rows] (vec (get-in default-sheet [:pipe-costs :rows])))
                         (assoc-in [:pipe-costs :rows 0 :nb] "aaa")
                         (assoc-in [:pipe-costs :rows 0 :soft] "aaa"))]
    
    (is (= {:pipe-costs ["missing required key"]}
           (:import/errors (schema/validate-network-model-ss no-pipe-costs-sheet))))
    (is (= nil (:import/errors (schema/validate-network-model-ss cols-need-coercing))))
    (is (= {:pipe-costs {:rows [{:nb ["should be a number"]}]}}
           (:import/errors (schema/validate-network-model-ss bad-fixed-col))))
    (is (= {:pipe-costs {:rows [{:soft ["should be a number"]}]}}
           (:import/errors (schema/validate-network-model-ss bad-variable-col))))
    (is (= {:pipe-costs {:rows [{:nb ["should be a number"] :soft ["should be a number"]}]}}
           (:import/errors (schema/validate-network-model-ss bad-both-col))))))


(deftest merge-errors-test
  (is (= nil (schema/merge-errors nil nil)))
  (is (= {:a 1} (schema/merge-errors {:a 1} nil)))
  (is (= {:a 1} (schema/merge-errors nil {:a 1})))
  (is (= {:a 1} (schema/merge-errors {:a 2} {:a 1})))
  (is (= {:a 1 :b 2} (schema/merge-errors {:b 2} {:a 1})))
  (is (= {:a [{:c 2} {:b [1 2] :e [4 5]} {:d 3}]} 
         (schema/merge-errors {:a [nil {:b [1 2]} {:d 3}]} {:a [{:c 2} {:e [4 5]} nil]}))))

(run-tests 'thermos-test.backend.spreadsheet.schema)
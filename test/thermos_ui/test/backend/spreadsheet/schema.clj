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
    
    (is (nil? (:errors (schema/validate-network-model-ss default-sheet))))
    (is (nil? (:errors (schema/validate-network-model-ss tarriff-with-string-unit-charge))))
    (is (nil? (:errors (schema/validate-network-model-ss tarriff-with-int-unit-charge))))))

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
    (is (= 
         {:tariffs ["missing required key"]}
         (:errors (schema/validate-network-model-ss no-tarriffs-sheet))))
    (is (= 
         {:tariffs {:rows [{:unit-rate ["missing required key"]}]}}
         (:errors (schema/validate-network-model-ss no-unit-rate))))
    (is (=
         {:tariffs {:rows [{:unit-rate ["should be a double"]}]}}
         (:errors (schema/validate-network-model-ss bad-unit-rate))))))

(run-tests 'thermos-test.backend.spreadsheet.schema)
(ns thermos-test.backend.spreadsheet.network-model
  (:require [clojure.test :refer :all]
            [thermos-specs.defaults :as defaults]
            [thermos-backend.spreadsheet.core :as ss-core]
            [thermos-backend.spreadsheet.common :as ss-common]
            [clojure.java.io :as io]))

(defn write-to-ss [doc]
  (let [out-sheet (ss-core/to-spreadsheet doc)
        baos (java.io.ByteArrayOutputStream.)]
    (ss-common/write-to-stream out-sheet baos)
    (.toByteArray baos)))

(defn read-to-doc [bytearray]
  (-> bytearray
      (io/input-stream)
      (ss-core/from-spreadsheet)))

(def initial-doc 
  (merge defaults/default-document
         #:thermos-specs.document
          {:connection-costs
           {0 #:thermos-specs.tariff
               {:id 0
                :name "cost-name"
                :fixed-connection-cost 123
                :variable-connection-cost 456}}
           
           :insulation
           {0 #:thermos-specs.measure
               {:id 0
                :name "insulation-name"
                :fixed-cost 1
                :cost-per-m2 2
                :maximum-effect 0.1
                :maximum-area 0.2
                :surface :roof}}
           
           :pumping-emissions {:co2 0.1 :pm25 0.2 :nox 0.3}
           :emissions-cost {:co2 0.4 :pm25 0.5 :nox 0.6}
           :emissions-limit {:co2 {:value 0.9 :enabled true} 
                             :pm25 {:value 0.8 :enabled true} 
                             :nox {:value 0.7 :enabled true}}}))

;; A single round-trip means all the IDs and row numbers change, plus the 
;; initial document contains lots of fields that aren't exported.
(deftest double-round-trip
  (is (=
       (-> initial-doc
           write-to-ss
           read-to-doc)
       (-> initial-doc
           write-to-ss
           read-to-doc
           write-to-ss
           read-to-doc))))

(deftest emissions
  (let [round-tripped (-> initial-doc write-to-ss read-to-doc)]
    (is (= (:pumping-emissions round-tripped) (:pumping-emissions initial-doc)))
    (is (= (:emissions-cost round-tripped) (:emissions-cost initial-doc)))
    (is (= (:emissions-limit round-tripped) (:emissions-limit initial-doc)))))

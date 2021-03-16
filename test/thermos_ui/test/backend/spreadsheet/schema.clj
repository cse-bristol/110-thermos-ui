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
    (is (= {:tariffs ["missing sheet from spreadsheet"]}
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

    (is (= {:pipe-costs ["missing sheet from spreadsheet"]}
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

(deftest supply-model
  (let [has-error
        (fn [errors ks v]
          (println errors)
          (let [errors (get-in errors ks)]
            (is (contains? (set errors) v) (prn-str "Error " v " not in: " errors))))

        bad-rows-per-day
        (-> default-sheet
            (assoc-in [:supply-day-types :rows] (vec (get-in default-sheet [:supply-day-types :rows])))
            (assoc-in [:supply-day-types :rows 0 :divisions] 23))

        bad-day-type-name
        (-> default-sheet
            (assoc-in [:supply-day-types :rows] (vec (get-in default-sheet [:supply-day-types :rows])))
            (assoc-in [:supply-day-types :rows 0 :name] "bad day"))

        bad-day-type-profile
        (-> default-sheet
            (assoc-in [:supply-profiles :rows] (vec (get-in default-sheet [:supply-profiles :rows])))
            (assoc-in [:supply-profiles :rows 0 :day-type] "bad day")
            (assoc-in [:supply-profiles :rows 0 :fuel-electricity-price] "bad day"))

        bad-fuel-name
        (-> default-sheet
            (assoc-in [:supply-plant :rows] (vec (get-in default-sheet [:supply-plant :rows])))
            (assoc-in [:supply-plant :rows 0 :fuel] "bad fuel"))

        bad-substations
        (-> default-sheet
            (assoc-in [:supply-substations :rows] (vec (get-in default-sheet [:supply-substations :rows])))
            (assoc-in [:supply-substations :rows 0 :name] "an unreferenced name"))

        duplicate-substation
        (-> default-sheet
            (assoc-in [:supply-substations :rows] (vec (get-in default-sheet [:supply-substations :rows])))
            (assoc-in [:supply-substations :rows 1] {:name     "A substation"
                                                     :headroom 41000
                                                     :alpha    0.7
                                                     :spreadsheet/row 1}))

        duplicate-day-type
        (-> default-sheet
            (assoc-in [:supply-day-types :rows] (vec (get-in default-sheet [:supply-day-types :rows])))
            (assoc-in [:supply-day-types :rows 5] {:divisions 24
                                                   :name      "Peak day"
                                                   :frequency 1
                                                   :spreadsheet/row 5}))]

    (has-error (:import/errors (schema/validate-supply-model-ss bad-rows-per-day))
               [:supply-day-types]
               "day type 'Normal weekday' has 23 divisions, but had 24 entries in sheet 'supply profiles'")

    (has-error (:import/errors (schema/validate-supply-model-ss bad-day-type-name))
               [:supply-day-types]
               "value 'bad day' referenced, but not defined in sheet 'Supply profiles'")

    (has-error (:import/errors (schema/validate-supply-model-ss bad-day-type-profile))
               [:supply-profiles]
               "value 'bad day' referenced, but not defined in sheet 'Supply day types'")

    (has-error (:import/errors (schema/validate-supply-model-ss bad-fuel-name))
               [:supply-plant]
               "fuel 'bad fuel' referenced, but pricing not defined in sheet 'Supply profiles'")

    (has-error (:import/errors (schema/validate-supply-model-ss bad-substations))
               [:supply-plant]
               "value 'A substation' referenced, but not defined in sheet 'Supply substations'")

    (has-error (:import/errors (schema/validate-supply-model-ss bad-substations))
               [:supply-profiles]
               "substation 'A substation' referenced, but not defined in sheet 'Supply substations'")

    (has-error (:import/errors (schema/validate-supply-model-ss duplicate-substation))
               [:supply-substations :header :name]
               "duplicate identifier: 'A substation'")

    (has-error (:import/errors (schema/validate-supply-model-ss duplicate-day-type))
               [:supply-day-types :header :name]
               "duplicate identifier: 'Peak day'")))
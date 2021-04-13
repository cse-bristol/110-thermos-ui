;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-test.backend.spreadsheet.schema
  (:require [clojure.test :refer :all]
            [thermos-backend.spreadsheet.schema :as schema]
            [thermos-specs.defaults :as defaults]
            [thermos-backend.spreadsheet.core :as ss-core]
            [thermos-backend.spreadsheet.common :as ss-common]
            [clojure.java.io :as io]))

(def initial-doc
  (merge defaults/default-document
         #:thermos-specs.document
          {:pumping-emissions {:co2 0.1 :pm25 0.2 :nox 0.3}
           :emissions-cost {:co2 0.4 :pm25 0.5 :nox 0.6}
           :emissions-limit {:co2 {:value 0.9 :enabled true}
                             :pm25 {:value 0.8 :enabled true}
                             :nox {:value 0.7 :enabled true}}}))

(def default-sheet
  (let [out-sheet (ss-core/to-spreadsheet initial-doc)
        baos (java.io.ByteArrayOutputStream.)]
    (ss-common/write-to-stream out-sheet baos)
    (ss-common/read-to-tables (io/input-stream (.toByteArray baos)))))

(defn has-error [errors ks v]
  (let [errors (get-in errors ks)]
    (is (contains? (set errors) v) (prn-str "Error " v " not in: " errors))))

(deftest schema-coercion-works
  (let [tarriff-with-string-unit-charge 
        (assoc-in default-sheet [:thermos-specs.document/tariffs 1 :unit-charge] "0.05"),
        
        tarriff-with-int-unit-charge
        (assoc-in default-sheet [:thermos-specs.document/tariffs 1 :unit-charge] 5),]
    
    (is (nil? (:import/errors (schema/validate-spreadsheet default-sheet))))
    (is (nil? (:import/errors (schema/validate-spreadsheet tarriff-with-string-unit-charge))))
    (is (nil? (:import/errors (schema/validate-spreadsheet tarriff-with-int-unit-charge))))))

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
           (:import/errors (schema/validate-spreadsheet no-tarriffs-sheet))))
    (is (= {:tariffs {:rows [{:unit-rate ["missing required key"]}]}}
           (:import/errors (schema/validate-spreadsheet no-unit-rate))))
    (is (= {:tariffs {:rows [{:unit-rate ["should be a double"]}]}}
           (:import/errors (schema/validate-spreadsheet bad-unit-rate))))))

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
           (:import/errors (schema/validate-spreadsheet no-pipe-costs-sheet))))
    (is (= nil (:import/errors (schema/validate-spreadsheet cols-need-coercing))))

    (is (= {:pipe-costs {:rows [{:nb ["should be a number"]}]}}
           (:import/errors (schema/validate-spreadsheet bad-fixed-col))))
    (is (= {:pipe-costs {:rows [{:soft ["should be a number"]}]}}
           (:import/errors (schema/validate-spreadsheet bad-variable-col))))
    (is (= {:pipe-costs {:rows [{:nb ["should be a number"] :soft ["should be a number"]}]}}
           (:import/errors (schema/validate-spreadsheet bad-both-col))))))

(deftest other-parameters
  (let [missing-param
        (-> default-sheet
            (assoc-in [:other-parameters :rows]
                      (pop (vec (get-in default-sheet [:other-parameters :rows])))))

        bad-value
        (-> default-sheet
            (assoc-in [:other-parameters :rows] (vec (get-in default-sheet [:other-parameters :rows])))
            (assoc-in [:other-parameters :rows 0 :value] "something"))

        extra-param
        (-> default-sheet
            (assoc-in [:other-parameters :rows] (vec (get-in default-sheet [:other-parameters :rows])))
            (assoc-in [:other-parameters :rows 0 :parameter] "something"))]
    (has-error (:import/errors (schema/validate-spreadsheet missing-param))
               [:other-parameters :header :parameter]
               "missing required value: 'Max supplies'")
    (has-error (:import/errors (schema/validate-spreadsheet bad-value))
               [:other-parameters :rows 0 :value]
               "should be either hot-water or saturated-steam")
    (has-error (:import/errors (schema/validate-spreadsheet extra-param))
               [:other-parameters :rows 0]
               "unknown parameter")))

(deftest supply-model
  (let [bad-rows-per-day
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
                                                   :spreadsheet/row 5}))

        bad-default-profile
        (-> default-sheet
            (assoc-in [:supply-parameters :rows] (vec (get-in default-sheet [:supply-parameters :rows])))
            (assoc-in [:supply-parameters :rows 6 :value] "non-existent profile"))]

    (is (= (:import/errors default-sheet) nil))
    (has-error (:import/errors (schema/validate-spreadsheet bad-rows-per-day))
               [:supply-day-types :header :name]
               "day type 'Normal weekday' has 23 divisions, but had 24 entries in sheet 'supply profiles'")

    (has-error (:import/errors (schema/validate-spreadsheet bad-day-type-name))
               [:supply-day-types :header :name]
               "value 'bad day' referenced, but not defined in sheet 'Supply profiles'")

    (has-error (:import/errors (schema/validate-spreadsheet bad-day-type-profile))
               [:supply-profiles :header :day-type]
               "value 'bad day' referenced, but not defined in sheet 'Supply day types'")

    (has-error (:import/errors (schema/validate-spreadsheet bad-fuel-name))
               [:supply-plant :header :fuel]
               "value 'bad fuel' referenced, but not defined in sheet 'Supply profiles'")

    (has-error (:import/errors (schema/validate-spreadsheet bad-substations))
               [:supply-plant :header :substation]
               "value 'A substation' referenced, but not defined in sheet 'Supply substations'")

    (has-error (:import/errors (schema/validate-spreadsheet bad-substations))
               [:supply-profiles :header :substation]
               "value 'A substation' referenced, but not defined in sheet 'Supply substations'")

    (has-error (:import/errors (schema/validate-spreadsheet duplicate-substation))
               [:supply-substations :header :name]
               "duplicate identifier: 'A substation'")

    (has-error (:import/errors (schema/validate-spreadsheet duplicate-day-type))
               [:supply-day-types :header :name]
               "duplicate identifier: 'Peak day'")

    (has-error (:import/errors (schema/validate-spreadsheet bad-default-profile))
               [:supply-parameters :header :default-profile]
               "value 'non-existent profile' referenced, but not defined in sheet 'Supply profiles'")))
;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-test.backend.spreadsheet.network-model
  (:require [clojure.test :refer :all]
            [thermos-specs.defaults :as defaults]
            [thermos-backend.spreadsheet.core :as ss-core]
            [thermos-backend.spreadsheet.common :as ss-common]
            [thermos-specs.document :as document]
            [thermos-specs.tariff :as tariff]
            [thermos-specs.supply :as supply]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.data :as data]
            [com.rpl.specter :as S]))

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
               {:cc-id 0
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

           :capital-costs
           {:connection {:annualize true
                         :recur true
                         :period 1
                         :rate 0.02}}

           :pumping-emissions {:co2 0.1 :pm25 0.2 :nox 0.3}
           :emissions-cost {:co2 0.4 :pm25 0.5 :nox 0.6}
           :emissions-limit {:co2 {:value 0.9 :enabled true}
                             :pm25 {:value 0.8 :enabled true}
                             :nox {:value 0.7 :enabled true}}}))

(defn- =? [o1 o2 & {:keys [key exclude-keys]}]
  (let [val
        (cond
          (and (map? o1) (map? o2))
          (every? true? (for [key (set/union (set (keys o1)) (set (keys o2)))
                              :when (not (contains? exclude-keys key))]
                          (=? (get o1 key) (get o2 key) :key key :exclude-keys exclude-keys)))

          (and (sequential? o1) (sequential? o2))
          (and (= (count o1) (count o2))
               (every? true?
                       (map-indexed
                        (fn [i item]
                          (=? item (get o2 i) :key i :exclude-keys exclude-keys)) o1)))

          (and (number? o1) (number? o2))
          (== o1 o2)

          (and (number? o1) (== o1 0) (nil? o2))
          true

          :else
          (= o1 o2))]
    (when-not val (println (str "difference at: " key) (data/diff o1 o2)))
    val))

(deftest round-trip
  (let [round-tripped (-> initial-doc write-to-ss read-to-doc)]
    (is (= nil (:import/errors round-tripped)))
    (is (=? (::document/capital-costs round-tripped) (::document/capital-costs initial-doc)))
    (is (=? (::document/connection-costs round-tripped) (::document/connection-costs initial-doc)))
    (is (=? (::document/pumping-cost-per-kwh round-tripped) (::document/pumping-cost-per-kwh initial-doc)))
    (is (=? (::document/pumping-overhead round-tripped) (::document/pumping-overhead initial-doc)))
    (is (=? (::document/pumping-emissions round-tripped) (::document/pumping-emissions initial-doc)))
    (is (=? (::document/ground-temperature round-tripped) (::document/ground-temperature initial-doc)))
    (is (=? (::document/return-temperature round-tripped) (::document/return-temperature initial-doc)))
    (is (=? (::document/flow-temperature round-tripped) (::document/flow-temperature initial-doc)))
    (is (=? (::document/npv-term round-tripped) (::document/npv-term initial-doc)))
    (is (=? (::document/insulation round-tripped) (::document/insulation initial-doc)))
    (is (=? (::document/objective round-tripped) (::document/objective initial-doc)))
    (is (=? (::document/maximum-runtime round-tripped) (::document/maximum-runtime initial-doc)))
    (is (=? (::document/npv-rate round-tripped) (::document/npv-rate initial-doc)))
    (is (=? (::document/medium round-tripped) (::document/medium initial-doc)))
    (is (=? (::document/loan-rate round-tripped) (::document/loan-rate initial-doc)))
    (is (=? (::document/pumping-emissions round-tripped) (::document/pumping-emissions initial-doc)))
    (is (=? (::document/emissions-cost round-tripped) (::document/emissions-cost initial-doc)))
    (is (=? (::document/loan-term round-tripped) (::document/loan-term initial-doc)))
    (is (=? (::document/emissions-limit round-tripped) (::document/emissions-limit initial-doc)))
    (is (=? (::document/mip-gap round-tripped) (::document/mip-gap initial-doc)))
    (is (=? (get-in round-tripped [::document/alternatives 0])
            (get-in initial-doc [::document/alternatives 1]) :exclude-keys #{::supply/id ::supply/cost-per-kwp}))
    (is (=? (get-in round-tripped [::document/tariffs 0])
            (get-in initial-doc [::document/tariffs 1]) :exclude-keys #{::tariff/id}))))

(defn- prep-pipe-costs [pipe-costs]
  (->> pipe-costs
       (S/transform [:rows S/MAP-KEYS double?] int)
       (S/transform [:rows S/MAP-VALS] #(dissoc % :losses-kwh :capacity-kw))
       (S/transform [:rows S/MAP-VALS S/MAP-VALS double?] int)
       (S/transform [:rows S/MAP-VALS S/MAP-KEYS int?] (:civils pipe-costs))))

(deftest pipe-costs
  ; fails on key :default-civils
  (let [round-tripped (-> initial-doc write-to-ss read-to-doc)]
    (is (= (prep-pipe-costs (::document/pipe-costs initial-doc))
           (prep-pipe-costs (::document/pipe-costs round-tripped))))))

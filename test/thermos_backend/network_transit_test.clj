;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.network-transit-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [thermos-backend.network-transit :as network-transit]))

(def document
  {:thermos-specs.document/candidates
   {"a" {:thermos-specs.candidate/id "a"
         :thermos-specs.candidate/inclusion :optional
         :thermos-specs.candidate/connections ["b" "c"]
         :thermos-specs.candidate/geometry "{\"type\":\"Point\",\"coordinates\":[1,2]}"}}
   :thermos-specs.document/npv-term 30
   :thermos-specs.document/mip-gap 0.05
   :thermos-specs.document/tags #{:x :y}
   :thermos-specs.document/pipe-costs {:rows {100 {:pipe 1.5}} :civils {1 "Soft"}}
   :thermos-specs.solution/objective ##NaN
   :thermos-specs.solution/ratio 1/4
   :thermos-specs.document/id #uuid "6d5f3c2e-5b8e-4c1a-9d0e-1a2b3c4d5e6f"})

(deftest round-trip
  (let [back (-> (pr-str document)
                 (network-transit/edn->transit)
                 (network-transit/transit->edn)
                 (edn/read-string))]
    (is (= (dissoc document :thermos-specs.solution/objective :thermos-specs.solution/ratio)
           (dissoc back :thermos-specs.solution/objective :thermos-specs.solution/ratio)))
    (is (Double/isNaN (:thermos-specs.solution/objective back)))
    (is (= 0.25 (:thermos-specs.solution/ratio back))
        "ratios, which the editor can't read, go as doubles")))


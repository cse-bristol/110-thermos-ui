;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.content-migrations.default-parameter-set-test
  (:require [thermos-backend.content-migrations.default-parameter-set :as sut]
            [thermos-backend.solver.interop :as interop]
            [clojure.test :as t]
            [thermos-specs.document :as document]
            [thermos-specs.supply :as supply]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.path :as path]
            [thermos-specs.tariff :as tariff]
            [thermos-specs.defaults :as defaults]
            [thermos-specs.parameters :as parameters]
            [thermos.opt.net.specs :as specs]))

(defn- street-document
  "A street of `n` buildings using the set's tables, each joined to the
  next by a 30m path, with an ASHP energy centre at the end."
  [n]
  (let [junction #(str "j" %)
        building (fn [i]
                   #::candidate{:id (str "b" i) :type :building :inclusion :optional
                                :selected false :connections [(junction i)]
                                ::demand/kwh 200000.0 ::demand/kwp 120.0
                                ::demand/connection-count 20
                                ::demand/counterfactual 1
                                ::demand/alternatives #{2}
                                ::tariff/id 2 ::tariff/cc-id 4})
        street   (fn [i]
                   #::candidate{:id (str "p" i) :type :path :inclusion :optional :selected false
                                ::path/start (junction i) ::path/end (junction (inc i))
                                ::path/length 30.0 ::path/civil-cost-id (inc (mod i 4))})
        supply   (assoc (building n) ::supply/technology-id 0)]
    (assoc (parameters/new-document defaults/default-document sut/content nil)
           ::document/candidates
           (into {} (map (juxt ::candidate/id identity))
                 (concat (map building (range n)) [supply] (map street (range n)))))))

(defn- model-input [doc]
  (#'interop/instance->model-input doc
                                   (interop/simplified-graph doc)
                                   (interop/doc->pipe-curves doc)
                                   (#'interop/make-market-decisions doc)))

(t/deftest the-set-makes-valid-problems
  (doseq [objective [:network :system]]
    (let [doc (-> (street-document 5)
                  (assoc ::document/objective objective
                         ::document/consider-alternatives true))]
      (t/is (specs/ensure-valid-problem (model-input doc)) (str objective)))))

(t/deftest fuels-have-every-day-type
  (let [day-types (set (keys (::supply/day-types defaults/default-document)))]
    (doseq [[_ fuel] (::supply/fuels sut/content)
            k [:price :co2]]
      (t/is (= day-types (set (keys (get fuel k))))))))

(t/deftest pipe-costs-follow-the-research
  ;; DN100 in carriageway is £450 pipe + £1,105 civils before the
  ;; on-costs, contingency and route allowance
  (let [row (get-in sut/content [::document/pipe-costs :rows 100.0])
        k (* 1.0835 1.3 1.1)]
    (t/is (= (Math/round (* k 450)) (:pipe row)))
    (t/is (= (Math/round (* k 1105)) (get row 3)))))

(t/deftest the-set-has-only-its-categories
  (t/is (= sut/categories (parameters/categories-in sut/content)))
  (t/is (not-any? #(contains? sut/content %) sut/solver-keys)))

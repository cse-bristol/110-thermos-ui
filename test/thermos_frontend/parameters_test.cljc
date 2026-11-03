;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.parameters-test
  "Tests for thermos-specs.parameters. They live here so that both the
  clj runner and the cljs :test build run them (and the inline tests in
  that namespace, for the cljs build)."
  (:require [clojure.test :refer [deftest testing is]]
            [thermos-specs.parameters :as parameters]
            [thermos-specs.document :as document]
            [thermos-specs.defaults :as defaults]
            [thermos-specs.supply :as supply]
            [thermos-specs.tariff :as tariff]))

(def network
  (-> defaults/default-document
      (assoc ::document/tariffs
             {1 #::tariff{:id 1 :name "Standard" :unit-charge 0.05}
              2 #::tariff{:id 2 :name "Cheap" :unit-charge 0.02}})
      (assoc ::document/candidates
             {"a" {::tariff/id 1}
              "b" {::tariff/id 2}
              "s" {::supply/technology-id 0}})))

(deftest extracting-a-set
  (let [set (parameters/extract network [:tariffs :supply-technologies])]
    (is (= #{:tariffs :supply-technologies} (::parameters/categories set)))
    (is (= (::document/tariffs network) (::document/tariffs set)))
    (is (contains? set ::tariff/market-term))
    (is (not (contains? set ::document/candidates)))
    (is (not (contains? set ::document/pipe-costs)))
    (is (= #{:tariffs :supply-technologies} (parameters/categories-in set)))))

(deftest applying-a-set
  (let [content {::parameters/categories #{:tariffs}
                 ::document/tariffs {7 #::tariff{:id 7 :name "Standard" :unit-charge 0.09}
                                     8 #::tariff{:id 8 :name "New" :unit-charge 0.1}}}
        doc (parameters/apply-set network content {:source {:id 3 :name "Set" :revision 4}})]
    (testing "tariffs with the same name are replaced, keeping their ID and links"
      (is (= 0.09 (get-in doc [::document/tariffs 1 ::tariff/unit-charge])))
      (is (= 1 (get-in doc [::document/candidates "a" ::tariff/id]))))
    (testing "tariffs not in the set are removed, and so are links to them"
      (is (not-any? #(= "Cheap" (::tariff/name %)) (vals (::document/tariffs doc))))
      (is (nil? (get-in doc [::document/candidates "b" ::tariff/id]))))
    (testing "new tariffs get a new ID, which they know"
      (let [[id t] (first (filter #(= "New" (::tariff/name (second %))) (::document/tariffs doc)))]
        (is (= id (::tariff/id t)))
        (is (not= 1 id))))
    (testing "other categories are untouched"
      (is (= (::document/pipe-costs network) (::document/pipe-costs doc)))
      (is (= (::document/supply-technologies network) (::document/supply-technologies doc))))
    (testing "the source is recorded"
      (is (= {:name "Set" :revision 4 :categories #{:tariffs}}
             (dissoc (get-in doc [::document/parameter-sources 3]) :applied))))))

(deftest applying-never-changes-heating-to-cooling
  (let [content (parameters/extract defaults/default-cooling-document [:objective])
        doc (parameters/apply-set network (assoc content ::document/npv-rate 0.1) {})]
    (is (= 0.1 (::document/npv-rate doc)))
    (is (= (::document/flow-temperature network) (::document/flow-temperature doc)))
    (is (not (document/is-cooling? doc)))))

(deftest changed-sources
  (let [doc {::document/parameter-sources
             {1 {:name "A" :revision 2}
              2 {:name "B" :revision 2 :dismissed-revision 3}
              3 {:name "C" :revision 2}}}]
    (is (= [{:id 1 :name "A" :revision 2 :current-revision 3}]
           (parameters/changed-sources doc {1 {:revision 3 :name "A"}
                                            2 {:revision 3 :name "B"}
                                            3 {:revision 2 :name "C"}})))
    (is (= [2] (map :id (parameters/changed-sources doc {2 {:revision 4}}))))))

(deftest new-documents
  (let [content {::parameters/categories #{:tariffs}
                 ::document/tariffs {0 #::tariff{:id 0 :name "X"}}}
        doc (parameters/new-document defaults/default-document content {:id 5 :name "S" :revision 1})]
    (is (= (::document/tariffs content) (::document/tariffs doc)))
    (is (= (::document/pipe-costs defaults/default-document) (::document/pipe-costs doc)))
    (is (= #{5} (set (keys (::document/parameter-sources doc)))))))

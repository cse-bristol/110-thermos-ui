(ns thermos-ui.test.frontend.operations
  (:require [clojure.test :refer :all]
            [thermos-ui.specs.document :as document]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.frontend.operations :as ops])
  )

(deftest selected-candidates-ids-works
  (let [document
        {::document/candidates
         {"a" {::candidate/id "a" ::candidate/selected false}
          "b" {::candidate/id "b" ::candidate/selected true}
          }}
        ]
    (is
     (= (ops/selected-candidates-ids document) #{"b"}))))

(deftest select-all-selects-all
  (let [document
        {::document/candidates
         {
          "a" {::candidate/id "a" ::candidate/selected false}
          "b" {::candidate/id "b" ::candidate/selected true}
          }}
        ]
    (is
     (= (ops/selected-candidates-ids (ops/select-all-candidates document))
        #{"a" "b"}))))

(deftest selection-modes-work
  (let [test-document
        (fn [a b]
          {::document/candidates
           {"a" {::candidate/id "a" ::candidate/selected a}
            "b" {::candidate/id "b" ::candidate/selected b}}})
        ]
    (testing "replace"
      (is (= (ops/select-candidates (test-document true false) #{"a"} :replace)
             (test-document true false)))
      (is (= (ops/select-candidates (test-document false true) #{"a"} :replace)
             (test-document true false))))

    (testing "union"
      (is (= (ops/select-candidates (test-document false false) #{"a" "b"} :union)
             (test-document true true)))
      (is (= (ops/select-candidates (test-document false false) #{"a"} :union)
             (test-document true false))))

    (testing "intersection"
      (is (= (ops/select-candidates (test-document false false) #{"a" "b"} :intersection)
             (test-document false false)))
      (is (= (ops/select-candidates (test-document true false) #{"a"} :intersection)
             (test-document true false)))
      (is (= (ops/select-candidates (test-document true false) #{"b"} :intersection)
             (test-document false false))))

    (testing "difference"
      (is (= (ops/select-candidates (test-document true true) #{"a" "b"} :difference)
             (test-document false false)))
      (is (= (ops/select-candidates (test-document true true) #{"a"} :difference)
          (test-document false true))))

    (testing "xor"
      (is (= (ops/select-candidates (test-document true false) #{"a" "b"} :xor)
             (test-document false true))))))


(deftest map-candidates-works
  (let [doc
        {:splarge 1

         ::document/candidates
         {"a" {::candidate/id "a" ::candidate/selected false}
          "b" {::candidate/id "b" ::candidate/selected true}
          }}]
    (is (= (ops/map-candidates
            doc
            #(assoc % ::candidate/selected :orange)
            )
           {:splarge 1
            ::document/candidates
         {"a" {::candidate/id "a" ::candidate/selected :orange}
          "b" {::candidate/id "b" ::candidate/selected :orange}
          }}
           ))
    )
  )

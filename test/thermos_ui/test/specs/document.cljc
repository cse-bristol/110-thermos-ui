(ns thermos-ui.test.specs.document
  (:require [thermos-ui.specs.document :as doc]
            [thermos-ui.specs.candidate :as can]
            [thermos-ui.specs.view :as view]
            [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [thermos-ui.test.specs.util]
            ))

(s/check-asserts true)

(deftest validate-an-empty-document
  (testing "That a document matches the spec"
    (is
     (valid?
      ::doc/document
      {::doc/candidates {}
       ::doc/technologies {}
       ::view/view-state
       {::view/selection #{}
        ::view/table-state {::view/sort-column ::can/candidate-type
                            ::view/sort-direction :asc
                            ::view/filters #{}}}

       })
     )))

(let [valid-supply
      {::can/candidate-id "a candidate"
       ::can/candidate-type :supply
       ::can/geometry 1
       ::can/name "1 Candidate street"
       ::can/postcode "BS3 1ED"
       ::can/building-type "Hospital"
       ::can/allowed-technologies #{}
       }
      ]

  (deftest fails-without-integrity
    (is (not (s/valid?
              ::doc/candidates
              {"test-one" valid-supply}))))

  (deftest works-with-integrity
    (is (valid? ::doc/candidates
                  {"test-one" (assoc valid-supply ::can/candidate-id "test-one")})))

  (deftest validate-a-candidate
    (testing "That a supply is a valid candidate"
      (is
       (valid? ::can/candidate valid-supply)))))

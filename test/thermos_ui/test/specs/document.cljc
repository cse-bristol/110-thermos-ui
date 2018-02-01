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
       {::view/bounding-box {:north 1 :south 1 :east 1 :west 1}
        ::view/table-state {::view/sort-column ::can/type
                            ::view/sort-direction :asc
                            ::view/filters #{}}}

       }))))

(let [valid-supply
      {::can/id "a candidate"
       ::can/type :supply
       ::can/geometry 1
       ::can/name "1 Candidate street"
       ::can/postcode "BS3 1ED"
       ::can/building-type "Hospital"
       ::can/allowed-technologies #{}
       ::can/inclusion :required
       ::can/selected false
       }]

  (deftest fails-without-integrity
    (is (not (s/valid?
              ::doc/candidates
              {"test-one" valid-supply}))))

  (deftest works-with-integrity
    (is (valid? ::doc/candidates
                {"test-one" (assoc valid-supply ::can/id "test-one")})))

  (deftest validate-a-candidate
    (testing "That a supply is a valid candidate"
      (is
       (valid? ::can/candidate valid-supply)))))

(deftest topological-validity-works
  (testing "Valid topology appears valid"
    (is (doc/is-topologically-valid
         {1 {::can/id 1 ::can/type :path ::can/path-start 2 ::can/path-end 3}
          2 {::can/id 2 ::can/type :demand}})))

  (testing "Paths may not end at the ID of another path"
    (is (not (doc/is-topologically-valid
              {1 {::can/id 1 ::can/type :path ::can/path-start 1 ::can/path-end 2}}))))

  (testing "Paths have to go from one place to another, different place"
    (is (not (doc/is-topologically-valid
              {1 {::can/id 1 ::can/type :path ::can/path-start 2 ::can/path-end 2}})))))

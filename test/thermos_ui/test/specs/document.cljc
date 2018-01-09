(ns thermos-ui.test.specs.document
  (:require [thermos-ui.specs.document :as doc]
            [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            ))

(s/check-asserts true)

(deftest validate-an-empty-document
  (testing "That a document matches the spec"
    (s/assert
     ::doc/document
     {::doc/candidates {}
      ::doc/technologies {}
      ::doc/view-state {}}
     )))

(deftest validate-a-candidate
  (testing "That a supply is a valid candidate"
    (s/assert
     ::doc/candidate
     {::doc/candidate-id "a candidate"
      ::doc/candidate-type :supply
      ::doc/geometry 1
      ::doc/name "1 Candidate street"
      ::doc/postcode "BS3 2AN"
      ::doc/building-type "Hospital"
      ::doc/allowed-technologies #{}
      }
     )
    )
  )

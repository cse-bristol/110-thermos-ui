(ns thermos-ui.test.specs.view
  (:require [thermos-ui.specs.view :as view]
            [thermos-ui.specs.document :as doc]
            [clojure.spec.alpha :as s]
            [clojure.test :refer :all]))

(s/check-asserts true)

(deftest validate-a-default-view
  (testing "That a view matches spec"
    (s/assert
     ::view/view-state
     {::view/selection {}
      ::view/table-state {::view/sort-column ::doc/candidate-id
                          ::view/sort-direction :asc
                          ::view/filters {}}
      ::view/map-state {}})))

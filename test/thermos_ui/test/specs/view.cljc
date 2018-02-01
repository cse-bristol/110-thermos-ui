(ns thermos-ui.test.specs.view
  (:require [thermos-ui.specs.view :as view]
            [thermos-ui.specs.candidate :as can]
            [clojure.spec.alpha :as s]
            [thermos-ui.test.specs.util]
            [clojure.test :refer :all]))

(s/check-asserts true)

(deftest validate-a-default-view
  (testing "That a view matches spec"
    (is (valid?
         ::view/view-state
         {::view/table-state {::view/sort-column ::can/id
                              ::view/sort-direction :asc
                              ::view/filters {}}
          ::view/bounding-box {:north 1 :south 1 :east 1 :west 1}}))))

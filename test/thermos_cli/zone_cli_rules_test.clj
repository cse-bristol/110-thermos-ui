(ns thermos-cli.zone-cli-rules-test
  (:require [clojure.test :refer [deftest is]]
            [thermos-cli.zone-cli-rules :as rules]))

(deftest test-assign-all-matching-values
  (is (= #{1 2 3}
         (:out (rules/assign-all-matching-values
                {"key" 1}
                [[[:in "key" 1] [1] :next]
                 [[:in "key" 1] [2] :next]
                 [[:in "key" 1] [3]]
                 [[:in "key" 1] [4]]]
                :out))))
  (is (= #{1 2 3}
         (:out (rules/assign-all-matching-values
                {"key" 1}
                [[[:in "key" 1] 1 :next]
                 [[:in "key" 1] 2 :next]
                 [[:in "key" 1] 3]
                 [[:in "key" 1] 4]]
                :out))))
  (is (= #{1 :next}
         (:out (rules/assign-all-matching-values
                {"key" 1}
                [[[:in "key" 1] 1 :next]
                 [[:in "key" 1] :next]
                 [[:in "key" 1] 3]]
                :out))))
  (is (= #{1}
         (:out (rules/assign-all-matching-values
                {"key" 1}
                [[[:in "key" 1] [1]]
                 [[:in "key" 1] [2] :next]
                 [[:in "key" 1] [3] :next]
                 [[:in "key" 1] [4] :next]]
                :out)))))
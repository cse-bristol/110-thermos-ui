(ns thermos-cli.zone-cli-rules-test
  (:require [clojure.test :refer [deftest is testing]]
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

(deftest test-is
  (testing "is should handle many ways of specifying `true`"
    (is (= true
           (rules/matches-rule? {"excluded" "true"} [:is "excluded"])
           (rules/matches-rule? {"excluded" "TRUE"} [:is "excluded"])
           (rules/matches-rule? {"excluded" "TRUe"} [:is "excluded"])
           (rules/matches-rule? {"excluded" "y"} [:is "excluded"])
           (rules/matches-rule? {"excluded" "Y"} [:is "excluded"])
           (rules/matches-rule? {"excluded" "1"} [:is "excluded"])
           (rules/matches-rule? {"excluded" 1.0} [:is "excluded"])
           (rules/matches-rule? {"excluded" true} [:is "excluded"])
           ))
    (is (= false
           (rules/matches-rule? {"excluded" "truey"} [:is "excluded"])
           (rules/matches-rule? {"excluded" "FALSE"} [:is "excluded"])
           (rules/matches-rule? {"excluded" "false"} [:is "excluded"])
           (rules/matches-rule? {"excluded" "n"} [:is "excluded"])
           (rules/matches-rule? {"excluded" ""} [:is "excluded"])
           (rules/matches-rule? {"excluded" "0"} [:is "excluded"])
           (rules/matches-rule? {"excluded" 0} [:is "excluded"])
           (rules/matches-rule? {"excluded" 0.0} [:is "excluded"])
           (rules/matches-rule? {"excluded" false} [:is "excluded"])
           (rules/matches-rule? {"excluded" nil} [:is "excluded"])
           ))))

(deftest ignore-rule-labels
  (testing "should ignore the labels of rules and just parse the rule body"
    (is (= true
           (rules/matches-rule? {"excluded" "true"} [:is "excluded"])
           (rules/matches-rule? {"excluded" "true"} [:rule/label "a rule" [:is "excluded"]])
           (rules/matches-rule? {"excluded" "true"} [:rule/label "a rule" 
                                                     [:rule/label "a rule" 
                                                      [:rule/label "a rule" [:is "excluded"]]]])))
    (is (= false
           (rules/matches-rule? {"excluded" "true"} [:not [:rule/label "a rule" [:is "excluded"]]])))))

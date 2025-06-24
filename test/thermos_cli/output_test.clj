(ns thermos-cli.output-test
  (:require [clojure.test :refer [deftest testing is]]))

;; for testing private function
(def output-type (intern 'thermos-cli.output 'output-type))

(deftest output-type-test
  (testing "Testing dispatch method")
  (is (= (output-type nil "test.gpkg") [:full :gpkg]))
  (is (= (output-type nil "summary.gpkg") [:full :gpkg]))
  (is (= (output-type nil "summary.json") [:summary :json])))


(clojure.test/run-tests 'thermos-cli.output-test)
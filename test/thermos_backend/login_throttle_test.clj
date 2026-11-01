;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.login-throttle-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [thermos-backend.config :as config]
            [thermos-backend.login-throttle :as throttle]))

(use-fixtures :each
  (fn [t]
    (throttle/clear!)
    (with-redefs [config/config {:login-delay-schedule [0 0 10 60]}]
      (t))
    (throttle/clear!)))

(def day (* 24 60 60 1000))

(defn- fail-times! [user n now]
  (dotimes [_ n] (throttle/failed! user now)))

(deftest schedule
  (testing "the free attempts"
    (is (= 0 (throttle/seconds-to-wait "a" 0)))
    (fail-times! "a" 2 0)
    (is (= 0 (throttle/seconds-to-wait "a" 0))))
  (testing "then the delays, the last repeating"
    (throttle/failed! "a" 0)
    (is (= 10 (throttle/seconds-to-wait "a" 0)))
    (throttle/failed! "a" 0)
    (is (= 60 (throttle/seconds-to-wait "a" 0)))
    (throttle/failed! "a" 0)
    (is (= 60 (throttle/seconds-to-wait "a" 0)))))

(deftest counting-down
  (fail-times! "a" 3 0)
  (is (= 10 (throttle/seconds-to-wait "a" 0)))
  (is (= 5 (throttle/seconds-to-wait "a" 5000)))
  (is (= 1 (throttle/seconds-to-wait "a" 9001)) "rounds up")
  (is (= 0 (throttle/seconds-to-wait "a" 10000))))

(deftest usernames
  (fail-times! "Someone@Example.com" 3 0)
  (is (= 10 (throttle/seconds-to-wait "someone@example.com" 0)) "ignores case")
  (is (= 0 (throttle/seconds-to-wait "someone-else" 0)) "one name doesn't affect another"))

(deftest success-resets
  (fail-times! "a" 3 0)
  (throttle/succeeded! "a")
  (is (= 0 (throttle/seconds-to-wait "a" 0)))
  (fail-times! "a" 2 0)
  (is (= 0 (throttle/seconds-to-wait "a" 0)) "counts from the start again"))

(deftest forgetting
  (fail-times! "a" 5 0)
  (is (= 60 (throttle/seconds-to-wait "a" 0)))
  (throttle/failed! "a" day)
  (is (= 0 (throttle/seconds-to-wait "a" day))
      "a failure a day later is the first again")
  (throttle/failed! "b" (* 3 day))
  (is (= #{"b"} (set (keys (:failures @@#'throttle/state))))
      "old entries are cleared out"))

(deftest empty-schedule
  (with-redefs [config/config {:login-delay-schedule []}]
    (fail-times! "a" 10 0)
    (is (= 0 (throttle/seconds-to-wait "a" 0)))))

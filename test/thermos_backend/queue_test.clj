;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.queue-test
  (:require [thermos-backend.queue :as queue]
            [clojure.test :as t]))

(def jobs
  [;; 20 minutes and 5 minutes left
   {:id 1 :state "running" :expected-runtime 1800 :elapsed 600 :waited 700}
   {:id 2 :state "running" :expected-runtime 600  :elapsed 300 :waited 400}
   ;; queued; the longest waiting is taken first
   {:id 3 :state "ready" :expected-runtime 1200 :waited 200}
   {:id 4 :state "ready" :expected-runtime 900  :waited 100}
   {:id 5 :state "ready" :expected-runtime 300  :waited 50}])

(defn- timing [capacity jobs id]
  (select-keys (queue/estimate-timing capacity jobs id) [:starts-in :finishes-in]))

(t/deftest running-job
  (t/is (= {:elapsed 600.0 :expected-runtime 1800.0 :starts-in 0.0 :finishes-in 1200.0}
           (queue/estimate-timing 2 jobs 1))))

(t/deftest queued-jobs
  ;; slots are free in 300s and 1200s.
  ;; job 3 takes the first from 300 to 1500, job 4 the other from 1200
  ;; to 2100, and job 5 the first again from 1500 to 1800
  (t/is (= {:starts-in 300.0 :finishes-in 1500.0}  (timing 2 jobs 3)))
  (t/is (= {:starts-in 1200.0 :finishes-in 2100.0} (timing 2 jobs 4)))
  (t/is (= {:starts-in 1500.0 :finishes-in 1800.0} (timing 2 jobs 5)))
  (t/is (= [1 2 3] (map #(:position (queue/estimate-timing 2 jobs %)) [3 4 5]))))

(t/deftest spare-capacity
  (t/is (= {:starts-in 0.0 :finishes-in 1200.0} (timing 10 jobs 3))))

(t/deftest priority
  ;; a lower :priority goes first, whenever it was queued
  (let [jobs (map #(cond-> % (= 5 (:id %)) (assoc :priority -1)) jobs)]
    (t/is (= {:starts-in 300.0 :finishes-in 600.0} (timing 2 jobs 5)))
    (t/is (= 1 (:position (queue/estimate-timing 2 jobs 5))))))

(t/deftest overrun-and-unknown
  (t/is (= 0.0 (:finishes-in (queue/estimate-timing
                              1 [{:id 1 :state "running" :expected-runtime 60 :elapsed 90}] 1))))
  (t/is (nil? (queue/estimate-timing 1 jobs 99))))

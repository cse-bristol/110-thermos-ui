;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.login-throttle
  "Slowing down guessing passwords. After each consecutive failed login for
  a username, it has to wait the next number of seconds in
  (config :login-delay-schedule) before trying again; the last number
  repeats. A successful login, or a day without failures, starts again.

  Failures are counted in memory, by username whether it exists or not, so
  that the responses don't say which usernames exist."
  (:require [clojure.string :as string]
            [thermos-backend.config :refer [config]]))

(def ^:private forget-after-ms
  "How long a username's failures are remembered without another"
  (* 24 60 60 1000))

(def ^:private sweep-every-ms
  "How often forgotten failures are cleared out"
  (* 60 1000))

;; {:failures {username {:count n :last ms}}, :swept ms}
(defonce ^:private state (atom {:failures {} :swept 0}))

(defn clear! [] (reset! state {:failures {} :swept 0}))

(defn- key-for [username] (string/lower-case (str username)))

(defn- now-ms [] (System/currentTimeMillis))

(defn- current
  "The failures remembered for `username` at time `now`, if any"
  [failures username now]
  (let [f (get failures (key-for username))]
    (when (and f (< (- now (:last f)) forget-after-ms))
      f)))

(defn- delay-ms
  "How long to wait after `n` consecutive failures"
  [n]
  (let [schedule (config :login-delay-schedule)]
    (if (or (empty? schedule) (< n 1))
      0
      (* 1000 (nth schedule (min (dec n) (dec (count schedule))))))))

(defn seconds-to-wait
  "How many seconds `username` must wait before trying to log in again"
  ([username] (seconds-to-wait username (now-ms)))
  ([username now]
   (if-let [{n :count last :last} (current (:failures @state) username now)]
     (let [remaining (- (+ last (delay-ms n)) now)]
       (if (pos? remaining)
         (long (Math/ceil (/ remaining 1000.0)))
         0))
     0)))

(defn failed!
  "Note a failed login for `username`"
  ([username] (failed! username (now-ms)))
  ([username now]
   (swap! state
          (fn [{:keys [failures swept] :as s}]
            (let [n (:count (current failures username now) 0)
                  failures (assoc failures (key-for username) {:count (inc n) :last now})]
              (if (> (- now swept) sweep-every-ms)
                {:swept now
                 :failures (into {}
                                 (filter (fn [[_ f]] (< (- now (:last f)) forget-after-ms)))
                                 failures)}
                (assoc s :failures failures)))))
   nil))

(defn succeeded!
  "Note a successful login for `username`, which forgets its failures"
  [username]
  (swap! state update :failures dissoc (key-for username))
  nil)

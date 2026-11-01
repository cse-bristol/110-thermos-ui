;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.passwords
  "What makes a password acceptable"
  (:require [thermos-backend.config :refer [config]]))

(defn min-length [] (config :min-password-length))

(defn password-problem
  "nil if `password` is acceptable, otherwise a message saying why not"
  [password]
  (let [n (min-length)]
    (when (< (count password) n)
      (str "Passwords must be at least " n " characters long."))))

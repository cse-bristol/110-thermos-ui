;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.passwords-test
  (:require [clojure.test :refer [deftest is]]
            [thermos-backend.config :as config]
            [thermos-backend.passwords :as passwords]))

(deftest minimum-length
  (with-redefs [config/config {:min-password-length 8}]
    (is (nil? (passwords/password-problem "12345678")))
    (is (nil? (passwords/password-problem "a much longer password")))
    (is (= "Passwords must be at least 8 characters long."
           (passwords/password-problem "1234567")))
    (is (passwords/password-problem ""))
    (is (passwords/password-problem nil))))

;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-specs.demand
  (:require [clojure.spec.alpha :as s]
            [thermos-specs.tariff :as tariff]))

(s/def ::demand
  (s/keys :req [::kwh ::kwp ::connection-count ::emissions]
          :opt [::tariff/id ::insulation ::alternatives ::counterfactual
                ::group]))

(s/def ::kwh (s/and number? pos?))
(s/def ::kwp (s/and number? pos?))

(s/def ::insulation (s/* number?))
(s/def ::alternatives (s/* number?))
(s/def ::counterfactual number?)
(s/def ::group any?)

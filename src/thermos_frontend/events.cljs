;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.events
  (:require [thermos-frontend.events.core]
            ;; this is lame
            [thermos-frontend.events.pipe-events]
            [thermos-frontend.events.candidate-events]))

(def handle thermos-frontend.events.core/handle)

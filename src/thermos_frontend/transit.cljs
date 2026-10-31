;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.transit
  "Network documents go between the editor and the server as transit json
  (see thermos-backend.network-transit), which is much quicker to read
  than EDN."
  (:refer-clojure :exclude [read])
  (:require [cognitect.transit :as transit]))

(def content-type "application/transit+json")

(defonce ^:private reader (transit/reader :json))
(defonce ^:private writer (transit/writer :json))

(defn read [s] (transit/read reader s))

(defn write [x] (transit/write writer x))

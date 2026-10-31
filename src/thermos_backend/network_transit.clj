;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.network-transit
  "Network documents are stored as EDN text (networks.content), but go
  between the server and the editor as transit, which the browser reads
  much faster than EDN."
  (:refer-clojure :exclude [read-string])
  (:require [cognitect.transit :as transit]
            [clojure.edn :as edn])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]))

(def ^:private as-double
  "Numbers which transit would send in a form the editor can't use"
  (transit/write-handler (constantly "d") double))

(def ^:private write-handlers
  {clojure.lang.Ratio    as-double
   clojure.lang.BigInt   as-double
   java.math.BigInteger  as-double
   java.math.BigDecimal  as-double})

(defn write-string
  "`data` as a transit json string"
  [data]
  (let [out (ByteArrayOutputStream. 4096)]
    (transit/write (transit/writer out :json {:handlers write-handlers}) data)
    (.toString out "UTF-8")))

(defn read-string
  "The data in transit json string `s`"
  [^String s]
  (transit/read (transit/reader (ByteArrayInputStream. (.getBytes s "UTF-8")) :json)))

(defn edn->transit
  "A network document stored as EDN, as transit json"
  [edn]
  (write-string (edn/read-string edn)))

(defn transit->edn
  "A network document sent as transit json, as EDN to store"
  [s]
  (binding [*print-length* nil
            *print-level* nil]
    (pr-str (read-string s))))

(ns thermos-test.backend.spreadsheet.supply-model
  (:require [clojure.test :refer :all]
            [thermos-specs.defaults :as defaults]
            [thermos-backend.spreadsheet.core :as ss-core]
            [thermos-backend.spreadsheet.common :as ss-common]
            [clojure.java.io :as io]
            [thermos-specs.supply :as supply]))

(defn flat [x]
       (let [vals (vec (repeat 24 x))]
         {0 vals 1 vals 2 vals 3 vals 4 vals}))

(def initial-doc
  (assoc-in defaults/default-document [:thermos-specs.supply/substations 0 :load-kw] (flat 1.23)))

(defn write-to-ss [doc]
  (let [out-sheet (ss-core/to-spreadsheet doc)
        baos (java.io.ByteArrayOutputStream.)]
    (ss-common/write-to-stream out-sheet baos)
    (.toByteArray baos)))

(defn read-to-doc [bytearray]
  (-> bytearray
      (io/input-stream)
      (ss-core/from-spreadsheet)))

(deftest plants
  (let [round-tripped (-> initial-doc write-to-ss read-to-doc)]
    (is (= (::supply/plants round-tripped) (::supply/plants initial-doc)) (:import/errors round-tripped))))

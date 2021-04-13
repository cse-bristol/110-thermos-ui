;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.changelog
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def changelog
  (-> (io/resource "changelog.edn")
      (io/reader)
      (slurp)
      (edn/read-string)))


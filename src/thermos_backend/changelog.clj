(ns thermos-backend.changelog
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def changelog
  (-> (io/resource "changelog.edn")
      (io/reader)
      (slurp)
      (edn/read-string)))


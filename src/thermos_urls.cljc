;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-urls
  (:require [clojure.string :as s]))

(def api-prefix "/api")

(defn document
  ([org doc]
   (str (s/join "/" [api-prefix "problem" org doc])))
  ([org doc id]
   (str (s/join "/" [api-prefix "problem" org doc id]))))

(defn run-status
  ([org doc id]
   (str (s/join "/" [api-prefix "problem" org doc id "status"]))))

(defn tile [x y z]
  (str (s/join "/" [api-prefix "map/candidates" z x y])))

(defn editor [org nam ver]
  (str (s/join "/" ["" org nam ver ""])))

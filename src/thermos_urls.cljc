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

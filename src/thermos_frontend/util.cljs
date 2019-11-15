(ns thermos-frontend.util)

(defn target-value [e]
  (.. e -target -value))

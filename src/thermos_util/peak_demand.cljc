(ns thermos-util.peak-demand)

(def peak-constant 21.84)
(def peak-gradient 0.0004963)

(defn annual->peak-demand [annual-demand]
  (+ peak-constant (* annual-demand peak-gradient)))

(defn peak->annual-demand [kwp]
  "In some data there is a known peak but no annual, and just point geometry.
  In this case, we can invert the peak regression to get an annual demand"
  [kwp]
  (max 0.0 (/ (- kwp peak-constant) peak-gradient)))

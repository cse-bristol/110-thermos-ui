(ns thermos-ui.frontend.state)

;; The document we are editing
(defonce doc
  (atom {}))

(def selected-candidates
  "An atom derived from `doc` which contains the IDs of selected candidates.
  This is a mirror of the ::candidate/selected value."
  nil)

(def spatial-index
  "An atom derived from `doc` which contains a spatial index (R-tree)
  of all the candidates in `doc`."
  nil)

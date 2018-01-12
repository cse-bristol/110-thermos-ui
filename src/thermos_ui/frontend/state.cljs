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

(s/def ::selection-method
  #{:replace :union :intersection :difference :xor})

(defn select-in-shape!
  "Change the selection for all candidates in the given SHAPE, using the METHOD."
  [shape method])

(defn select!
  "Change the selection for all candidates with IDs in the CANDIDATE-IDS using the METHOD."
  [candidate-ids method])

(defn set-inclusion!
  "Change the inclusion constraint for the given candidates"
  [candidate-ids new-constraint])

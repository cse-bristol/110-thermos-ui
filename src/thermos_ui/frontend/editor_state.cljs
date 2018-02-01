(ns thermos-ui.frontend.editor-state
  (:require [thermos-ui.frontend.spatial :as spatial]
            [reagent.core :as reagent :refer [atom]]))

;; The document we are editing
(defonce state (atom {}))

(defn edit!
  "Change the state with f and any arguments.
  Also updates the spatial index data for the state."
  [document f & args]
  (apply swap! document (comp spatial/update-index f) args))

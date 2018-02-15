(ns thermos-ui.frontend.editor-state
  (:require [thermos-ui.frontend.spatial :as spatial]
            [thermos-ui.frontend.io :as io]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.frontend.operations :as operations]
            [reagent.core :as reagent :refer [atom]]))

;; The document we are editing
(defonce state (atom {}))

(defn edit!
  "Change the state with f and any arguments.
  Also updates the spatial index data for the state."
  [document f & args]
  (apply swap! document (comp spatial/update-index f) args))

(defn- feature->candidate
  "Convert a GEOJSON feature into a candidate map"
  [feature]
  ;; TODO this is not right
  (let [geometry (.-geometry feature)
        properties (js->clj (.-properties feature) :keywordize-keys true)]
    {::candidate/id (:id properties)
     ::candidate/geometry geometry}))


(defn load-tile! [document x y z]
  (io/request-geometry
   x y z
   (fn [json]
     ;; at the moment this is an object with a 'features' property
     ;; each feature is a thing that can go on the map
     ;; it has a geometry property and a properties property

     ;; 1: convert each feature into a candidate map

     (let [features (.-features json)
           candidates (into () (.map features feature->candidate))]
       ;; 2: update the document to contain the new candidates
       (edit! document
              operations/insert-candidates
              candidates)
       ))))

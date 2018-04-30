(ns thermos-ui.frontend.editor-state
  (:require [goog.object :as o]

            [thermos-ui.frontend.spatial :as spatial]
            [thermos-ui.frontend.io :as io]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.specs.document :as document]
            [thermos-ui.frontend.operations :as operations]
            [reagent.core :as reagent :refer [atom]]))

;; The document we are editing
(defonce state (atom {}))

(defn edit!
  "Update the document, but please do not change the spatial details this way!"
  [document f & args]
  (apply swap! document f args))

(defn edit-geometry!
  "Change the state with f and any arguments.
  Also updates the spatial index data for the state."
  [document f & args]
  (apply swap! document (comp spatial/update-index f) args))

(defn- feature->candidate
  "Convert a GEOJSON feature into a candidate map"
  [feature]

  (let [geometry (o/get feature "geometry")
        properties (o/get feature "properties")
        simple-geometry (o/get properties "simple_geometry" geometry)

        properties (dissoc (js->clj properties :keywordize-keys true) :simple_geometry)

        type (keyword (:type properties))
        ]

    (merge
     {::candidate/id (:id properties)
      ::candidate/name (:name properties)
      ::candidate/type type
      ::candidate/subtype (:subtype properties)
      ::candidate/geometry geometry
      ::candidate/simple-geometry simple-geometry
      ::candidate/inclusion :forbidden}
     (case type
       :path {::candidate/length (:length properties)
              ::candidate/path-start (:start-id properties)
              ::candidate/path-end (:end-id properties)}
       :demand {::candidate/demand (:demand properties)
                ::candidate/connection (:connect_id properties)}
       :supply {::candidate/connection (:connect_id properties)}
       ))))

(defn load-document! [org-name proj-name doc-version]
  (io/load-document
   org-name proj-name doc-version
   #(edit-geometry! state operations/load-document %)))

(defn save-document! [org-name proj-name cb]
  (let [state @state]
    (io/save-document
     org-name proj-name
     (document/keep-interesting state)
     cb
     )))

(defn load-tile! [document x y z]
  (io/request-geometry
   x y z
   (fn [json]
     ;; at the moment this is an object with a 'features' property
     ;; each feature is a thing that can go on the map
     ;; it has a geometry property and a properties property

     ;; 1: convert each feature into a candidate map

     (let [features (o/get json "features")
           candidates (into () (.map features feature->candidate))]
       ;; 2: update the document to contain the new candidates
       (edit-geometry! document
              operations/insert-candidates
              candidates)
       ))))

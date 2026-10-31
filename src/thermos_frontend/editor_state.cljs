;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.editor-state
  "Setting up the editor's state, which is re-frame's app-db holding the
  document (see thermos-frontend.db), and loading candidates for the map."
  (:require [goog.object :as o]
            [clojure.string :as string]
            [re-frame.core :as rf]
            [re-frame.db]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.cooling :as cooling]
            [thermos-specs.path :as path]
            [thermos-frontend.db :as db]
            [thermos-frontend.io :as io]
            [thermos-frontend.operations :as operations]
            [thermos-frontend.events]
            [thermos-frontend.fx]))

(def state
  "The document, as a reagent atom. Change it with events, not swap!."
  re-frame.db/app-db)

(defn needs-save? [] (db/needs-save? @state))

(defn- mark-unsaved-changes [_ _ old-db new-db]
  (when (db/unsaved-change? old-db new-db)
    (swap! state assoc-in [::db/server :needs-save] true)))

(defonce initialise
  (do (reset! state (db/initial-db))
      ;; a watch rather than an interceptor, since much of the editor
      ;; still changes the document without events
      (add-watch state ::mark-unsaved-changes #(mark-unsaved-changes %1 %2 %3 %4))
      (rf/dispatch [:server/poll])
      true))

(defn- feature->candidate
  "Convert a GEOJSON feature into a candidate map"
  [feature]

  (let [geometry (o/get feature "geometry")
        properties (o/get feature "properties")

        type (if (o/get properties "is-building" false)
               :building :path)

        height (o/get properties "height" nil)

        user-fields (cond-> (js->clj (o/get properties "user-fields" {}))
                      (and height (pos? height)) (assoc "Height" height))

        basics {::candidate/id (o/get properties "id")
                ::candidate/type type
                ::candidate/geometry geometry
                ::candidate/inclusion :forbidden
                ::candidate/user-fields user-fields}
        ]

    (case type
      :path
      (assoc basics
             ::path/length        (o/get properties "length")
             ::path/start         (o/get properties "start-id")
             ::path/end           (o/get properties "end-id"))

      :building
      (assoc basics
             ::demand/group            (o/get properties "conn-group")

             ::candidate/wall-area     (o/get properties "wall-area" 0)
             ::candidate/ground-area   (o/get properties "ground-area" 0)
             ::candidate/roof-area     (o/get properties "ground-area" 0)
             ::demand/kwh              (o/get properties "demand-kwh-per-year" nil)
             ::demand/kwp              (o/get properties "demand-kwp" nil)
             ::demand/connection-count (o/get properties "connection-count" 1)
             ::cooling/kwh             (o/get properties "cooling-kwh-per-year" nil)
             ::cooling/kwp             (o/get properties "cooling-kwp" nil)
             ::demand/source           (o/get properties "demand-source" nil)
             ::candidate/connections   (string/split (o/get properties "connection-ids") #",")))))

(defn load-tile! [x y z]
  (io/request-geometry
   x y z
   (fn [json]
     ;; at the moment this is an object with a 'features' property
     ;; each feature is a thing that can go on the map
     ;; it has a geometry property and a properties property

     ;; 1: convert each feature into a candidate map

     (let [features (o/get json "features")
           candidates
           (into
            ()
            (.map features
                  (fn [feature]
                    [(-> feature
                         (o/get "properties")
                         (o/get "id"))

                     (delay (feature->candidate feature))])))
           ]

       ;; 2: update the document to contain the new candidates
       (rf/dispatch [:doc/edit-geometry operations/insert-candidates candidates])))))

(ns thermos-ui.frontend.editor-state
  (:require [goog.object :as o]

            [thermos-ui.frontend.spatial :as spatial]
            [thermos-ui.frontend.io :as io]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.specs.document :as document]
            [thermos-ui.specs.defaults :refer [default-document]]
            [thermos-ui.frontend.operations :as operations]
            [reagent.core :as reagent :refer [atom]]))

;; The document we are editing
(defonce state (atom default-document))

(defonce run-state (atom
                    {:last-state nil
                     :needs-save nil
                     :last-load nil}))

(defonce run-state-timer (atom nil))

(declare maybe-update-run-state)

(defn start-run-state-timer []
  (swap! run-state-timer
         (fn [t]
           (or t
               (js/setInterval
                maybe-update-run-state
                1500)))))

(defn cancel-run-state-timer []
  (swap! run-state-timer
         (fn [t]
           (when t (js/clearInterval t))
           nil)))

(defn is-running? [] (:last-state @run-state))
(defn queue-position [] (:after @run-state))
(defn needs-save? [] (:needs-save @run-state))

(defn get-last-save [] (:last-load @run-state))

(defn update-run-state []
  (let [{[org proj id] :last-load} @run-state]
    (io/get-run-status
     org proj id
     (fn [result]
       (let [state (keyword (get result "state"))
             after (get result "after")]
         (swap! run-state assoc
                :last-state state
                :after after)
         (when (#{:queued :running} state)
           (start-run-state-timer)))))))

(defn maybe-update-run-state []
  (let [{last-state :last-state last-load :last-load} @run-state]
    (if (and last-load
             (or (nil? last-state)
                 (#{:queued :running} last-state)))
      (update-run-state)
      (cancel-run-state-timer))))

(defn edit!
  "Update the document, but please do not change the spatial details this way!"
  [document f & args]
  (if (needs-save?)
    (apply swap! document f args)
    (let [old-value @document
          new-value (apply swap! document f args)]
      (when (not= (document/keep-interesting old-value)
                  (document/keep-interesting new-value))
        (swap! run-state assoc :needs-save true))
      new-value)))

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
;;        simple-geometry (o/get properties "simple_geometry" geometry)
        
        empty->nil #(if (or (nil? %) (= "" %)) nil %)

        type (keyword (o/get properties "type" "demand"))
        name (empty->nil (o/get properties "name"))
        subtype (empty->nil (o/get properties "subtype"))

        basics {::candidate/id (o/get properties "id")
                ::candidate/name name
                ::candidate/type type
                ::candidate/subtype subtype
                ::candidate/geometry geometry
                ::candidate/inclusion :forbidden}
        ]
    
    (case type
      :path (assoc basics
                   ::candidate/length (o/get properties "length")
                   ::candidate/path-cost (o/get properties "cost")
                   ::candidate/path-start (o/get properties "start_id")
                   ::candidate/path-end (o/get properties "end_id"))
      :demand (assoc basics
                     ::candidate/demand (o/get properties "demand")
                     ::candidate/connection (o/get properties "connection_id"))
      :supply (assoc basics
                     ::candidate/connection (o/get properties "connection_id"))
      )))

(defn load-document! [org-name proj-name doc-version cb]
  (io/load-document
   org-name proj-name doc-version
   #(do
      (swap! run-state assoc
             :last-load [org-name proj-name doc-version]
             :needs-save nil)
      (maybe-update-run-state)
      (edit-geometry! state operations/load-document %)
      (cb))))

(defn save-document! [org-name proj-name run cb]
  (let [state @state]
    (io/save-document
     org-name proj-name
     (document/keep-interesting state)
     run
     #(do (swap! run-state assoc
                 :last-load [org-name proj-name %]
                 :needs-save nil)
          (cb org-name proj-name %)
          (update-run-state)))))

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

(ns thermos-frontend.editor-state
  (:require [goog.object :as o]

            [thermos-frontend.spatial :as spatial]
            [thermos-frontend.io :as io]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.path :as path]

            [clojure.string :as string]
            
            [thermos-specs.document :as document]
            [thermos-specs.defaults :refer [default-document]]
            [reagent.core :as reagent :refer [atom]]

            [thermos-frontend.operations :as operations]

            ))

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

(defonce watch-state-for-save
  (add-watch state
             :watch-for-save
             (fn [_ _ old-state new-state]
               (when-not (needs-save?)
                 (let [old-state (document/keep-interesting old-state)
                       new-state (document/keep-interesting new-state)]
                   (swap! run-state assoc :needs-save
                          (not= old-state new-state)))))))

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
        
        empty->nil #(if (or (nil? %) (= "" %)) nil %)

        type (if (o/get properties "is_building" false)
               :building :path)
        name (empty->nil (o/get properties "name"))
        subtype (empty->nil (o/get properties "type"))

        basics {::candidate/id (o/get properties "id")
                ::candidate/name name
                ::candidate/type type
                ::candidate/subtype subtype
                ::candidate/geometry geometry
                ::candidate/inclusion :forbidden}
        ]
    
    (case type
      :path
      (assoc basics
             ::path/length     (o/get properties "length")
             ::path/cost-per-m (o/get properties "unit_cost")
             ::path/start      (o/get properties "start_id")
             ::path/end        (o/get properties "end_id"))
      :building
      (assoc basics
             ::demand/kwh              (o/get properties "demand_kwh_per_year" nil)
             ::demand/kwp              (o/get properties "demand_kwp" nil)
             ::demand/connection-count (o/get properties "connection_count" 1)
             ::candidate/connections   (string/split (o/get properties "connection_ids") #",")))))

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

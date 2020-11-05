(ns thermos-frontend.editor-state
  (:require [goog.object :as o]
            [goog.net.XhrIo :as xhr]
            [thermos-frontend.spatial :as spatial]
            [thermos-frontend.io :as io]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.cooling :as cooling]
            [thermos-specs.path :as path]

            [clojure.string :as string]
            
            [thermos-specs.document :as document]
            [thermos-specs.defaults :refer [default-document default-cooling-document]]
            [thermos-frontend.preload :as preload]
            [reagent.core :as reagent :refer [atom]]

            [thermos-frontend.operations :as operations]
            [thermos-frontend.flow :as flow]
            [thermos-frontend.events :as events]

            [thermos-frontend.flow :as f]))

(defonce save-state
  (let [[_ project-id map-id network-id]
        (re-find #"/project/(\d+)/map/(\d+)/net/(new|\d+)"
                 (.-pathname js/window.location))
        project-id (js/parseInt project-id)
        map-id (js/parseInt map-id)
        network-id (if (= "new" network-id) :new (js/parseInt network-id))
        ]
    (atom {:project-id project-id
           :map-id map-id
           :network-id network-id
           :needs-save false
           :needs-load (not= :new network-id)
           :queue-position 0
           :run-state nil})))

(defn- update-url [new-url]
  (let [[_ project-id map-id network-id]
        (re-find #"/project/(\d+)/map/(\d+)/net/(new|\d+)" new-url)
        project-id (js/parseInt project-id)
        map-id (js/parseInt map-id)
        network-id (if (= "new" network-id) :new (js/parseInt network-id))]
    (swap! save-state
           assoc
           :project-id project-id
           :map-id map-id
           :network-id network-id
           :needs-load (not= :new network-id))
    (when-not (= (.-pathname js/window.location) new-url)
      (js/window.history.replaceState nil "Editor" new-url))))

(update-url (.-pathname js/window.location))

(defonce state
  (atom
   (let [p (preload/get-value :initial-state :clear true)]
     (if p
       (spatial/update-index (cljs.reader/read-string p))
       (operations/move-map
        (case (preload/get-value :mode)
          :cooling default-cooling-document
          default-document)
        
        
        (let [bounds (preload/get-value :map-bounds)]
          {:north (:y-max bounds)
           :south (:y-min bounds)
           :east  (:x-max bounds)
           :west  (:x-min bounds)}))))))

(def flow
  (flow/create-root
   {:state state
    :handler events/handle}))

(defn fire-event!
  "Handle event `e` on the next tick"
  [e]
  (flow/fire! flow e))

(set! js/thermos_initial_state nil)

(def running-state? #{:ready :running :cancel :cancelling})
(defn is-running? [] (running-state? (:run-state @save-state)))
(defn needs-save? [] (:needs-save @save-state))
(defn queue-position [] (:queue-position @save-state))

(defonce watch-state-for-save
  (add-watch
   state
   :watch-for-save
   (fn [_ _ old-state new-state]
     (when-not (needs-save?)
       (let [old-state (document/keep-interesting old-state)
             new-state (document/keep-interesting new-state)]
         (swap! save-state assoc :needs-save
                (not= old-state new-state)))))))

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

        type (if (o/get properties "is-building" false)
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
             ::path/length        (o/get properties "length")
             ::path/start         (o/get properties "start-id")
             ::path/end           (o/get properties "end-id"))

      :building
      (assoc basics
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
              candidates)))))

(declare poll!)

(defn- get-run-state [e]
  (let [s (.getResponseHeader (.. e -target) "x-run-state")]
    (when s
      (keyword (.substring s 1)))))

(defn- get-queue-position [e]
  (let [s (.getResponseHeader (.. e -target) "x-queue-position")]
    (when s (js/parseInt s))))

(defn save!
  "Save the current state to the server."
  [title & {:keys [run callback]}]

  (xhr/send
   (if run (str "?run=" (name run)) "")
   (fn on-success [e]
     (update-url
      (.getResponseHeader (.. e -target) "Location"))
     
     (swap! save-state
            assoc
            :needs-save false
            :needs-load false
            :run-state (get-run-state e)
            :queue-position (get-queue-position e))
     

     (when run (poll!))
     (when callback (callback)))

   "POST"
   (let [data (js/FormData.)
         doc (pr-str (document/keep-interesting @state))
         blob (js/Blob. #js [doc] #js {"type" "application/edn"})]
     (.append data "name" title)
     (.append data "content" blob "content.edn")
     data)))

(defn load!
  "Try and load the state associated with the save-state."
  [& {:keys [callback]}]
  (when (not= :new (:network-id @save-state))
    (xhr/send
     ""
     (fn on-success [e]
       (let [new-state (->> (.-target e)
                            (.getResponseText)
                            (cljs.reader/read-string))]
         (edit-geometry! state operations/load-document new-state)
         (swap! save-state
                assoc
                :needs-save false
                :needs-load false
                :run-state (get-run-state e)
                :queue-position (get-queue-position e))
         
         (when callback (callback))()))
     "GET"
     nil
     #js {"Accept" "application/edn"})))

(defonce poll-timer (atom nil))

(defn poll! []
  (xhr/send
   ""
   (fn on-success [e]
     (let [last-run-state (:run-state @save-state)
           run-state (get-run-state e)]
       (swap! save-state assoc
              :run-state run-state
              :queue-position (get-queue-position e))
       (case run-state
         :completed
         (case last-run-state
           (:ready :running)
           (do
             (swap! poll-timer
                (fn [t]
                  (when t (js/window.clearTimeout t))
                  nil))

             ;; TODO switch tab
             ;; (let [run-state (state/is-running?)
             ;;       last-state @last-run-state]
             ;;   (when (and (= :completed run-state)
             ;;              (or (= :running last-state)
             ;;                  (= :ready last-state)))
             ;;     (let [[org-name proj-name version] (state/get-last-save)]
             ;;       (state/load-document!
             ;;        org-name proj-name version
             ;;        (fn []
             ;;          (state/edit! state/state
             ;;                       assoc-in
             ;;                       [::view/view-state ::view/selected-tab]
             ;;                       :solution)))))
             
             (load!))

           nil)
         

         (:ready :running :cancelling :cancel)
         (swap! poll-timer
                (fn [t]
                  (when t (js/window.clearTimeout t))
                  (js/window.setInterval poll! 1000)))

         nil)))
   
   "HEAD"))

(poll!)

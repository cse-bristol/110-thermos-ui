;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.db
  "The shape of re-frame's app-db for the editor.

  app-db is the network document itself, as described by thermos-specs.
  Anything else the editor keeps in there goes under keywords outside the
  thermos-specs namespaces, which document/keep-interesting strips off,
  so it is never saved and never makes the document look modified:

  ::server  where the document lives on the server and the state of any
            run, see `initial-server`
  ::undo    the edits which can be undone and redone, see
            thermos-frontend.undo
  ::compare another network to compare this one with, see
            `comparing?` and thermos-frontend.comparison"
  (:require [thermos-frontend.transit :as transit]
            [thermos-specs.defaults :refer [default-document default-cooling-document]]
            [thermos-specs.document :as document]
            [thermos-frontend.operations :as operations]
            [thermos-frontend.preload :as preload]
            [thermos-frontend.spatial :as spatial]))

(def running-states #{:ready :running :cancel :cancelling})

(defn parse-network-url
  "The project, map and network ids in an editor url (or path), where the
  network id is :new for a network which has never been saved"
  [url]
  (when-let [[_ project-id map-id network-id]
             (re-find #"/project/(\d+)/map/(\d+)/net/(new|\d+)" url)]
    {:project-id (js/parseInt project-id)
     :map-id     (js/parseInt map-id)
     :network-id (if (= "new" network-id) :new (js/parseInt network-id))}))

(defn network-url
  "The editor's path for the network described by `server` (as in
  ::server), with `suffix` on the end"
  ([server] (network-url server nil))
  ([{:keys [project-id map-id network-id]} suffix]
   (str "/project/" project-id "/map/" map-id "/net/"
        (cond-> network-id (keyword? network-id) (name))
        suffix)))

(defn copy-name
  "A name for a copy of the network called `name`, which is not one of
  `taken`, made by putting a number in brackets on the end (or a bigger
  one than is there), and where in it the brackets are, as [new-name
  start end]"
  [name taken]
  (let [taken  (set taken)
        [_ base n] (re-matches #"(.*) \((\d+)\)" name)
        base   (or base name)
        start  (if n (inc (js/parseInt n)) 2)
        n      (first (remove #(taken (str base " (" % ")")) (iterate inc start)))
        copy   (str base " (" n ")")]
    [copy (count base) (count copy)]))

(defn parameter-set
  "In the parameter set editor, {:id :scope :project-name :categories} for
  the set being edited; otherwise nil"
  []
  (preload/get-value :parameter-set))

(defn initial-server
  "The ::server part of app-db for the editor at `path`"
  [path]
  (let [{:keys [network-id] :as ids} (parse-network-url path)]
    (assoc ids
           ;; a network which has never been saved is unsaved
           :needs-save (= :new network-id)
           :needs-load (number? network-id)
           ;; the name it was last saved as (the nav bar has the name
           ;; being edited)
           :name (preload/get-value :name)
           :queue-position 0
           :run-state nil
           :revision 0)))

(defn initial-document
  "The document the page was served with, or a new one covering the map"
  []
  (let [initial-state (js/document.getElementById "thermos-network")
        ;; only a new network is given the bounds of its map
        bounds (preload/get-value :map-bounds)]
    (cond-> (if initial-state
              ;; a saved network, a parameter set, or a new network
              ;; with the user's default parameters
              (spatial/update-index (transit/read (.-textContent initial-state)))
              (case (preload/get-value :mode)
                :cooling default-cooling-document
                default-document))
      bounds
      (operations/move-map
       {:north (:y-max bounds)
        :south (:y-min bounds)
        :east  (:x-max bounds)
        :west  (:x-min bounds)}))))

(defn initial-db []
  (assoc (initial-document)
         ::server (initial-server (.-pathname js/window.location))))

(defn server [db] (::server db))

(defn needs-save? [db] (:needs-save (server db)))

(defn saved
  "`db` after saving or loading, with `server-state` merged into ::server"
  [db server-state]
  (update db ::server
          #(-> (merge % server-state)
               (assoc :needs-save false :needs-load false)
               (update :revision inc))))

(defn unsaved-change?
  "Whether going from `old-db` to `new-db` makes the document need saving.
  Saving and loading change the document to match the server, so they
  increase the ::server :revision to say that their changes don't."
  [old-db new-db]
  (and (not (needs-save? new-db))
       (= (:revision (server old-db)) (:revision (server new-db)))
       (document/interesting-change? old-db new-db)))

(defn running-or-queued? [db] (contains? running-states (:run-state (server db))))

;; Comparing with another network. ::compare is
;;
;; {:networks [network ...]  the networks on the map to choose from
;;                           (see projects/map-networks), once fetched
;;  :loading  network        the network being fetched to compare with
;;  :network  network        the network being compared with
;;  :document document       its document, with its own spatial index
;;  :picked   #{id ...}}     its candidates last clicked on in the map
;;
;; and it stays while the document is edited, run or reloaded.

(defn comparison [db] (::compare db))

(defn comparing? [db] (some? (:document (comparison db))))

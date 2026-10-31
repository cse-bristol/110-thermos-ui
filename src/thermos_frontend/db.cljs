;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.db
  "The shape of re-frame's app-db for the editor.

  app-db is the network document itself, as described by thermos-specs.
  Anything else the editor keeps in there goes under keywords outside the
  thermos-specs namespaces, which document/keep-interesting strips off,
  so it is never saved and never makes the document look modified:

  ::server  where the document lives on the server and the state of any
            run, see `initial-server`"
  (:require [cljs.reader :as reader]
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

(defn initial-server
  "The ::server part of app-db for the editor at `path`"
  [path]
  (let [{:keys [network-id] :as ids} (parse-network-url path)]
    (assoc ids
           ;; a network which has never been saved is unsaved
           :needs-save (= :new network-id)
           :needs-load (not= :new network-id)
           :queue-position 0
           :run-state nil
           :revision 0)))

(defn initial-document
  "The document the page was served with, or a new one covering the map"
  []
  (if-let [initial-state (preload/get-value :initial-state :clear true)]
    (spatial/update-index (reader/read-string initial-state))
    (operations/move-map
     (case (preload/get-value :mode)
       :cooling default-cooling-document
       default-document)
     (let [bounds (preload/get-value :map-bounds)]
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

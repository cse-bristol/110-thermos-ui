;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.undo
  "Undo and redo for the editor. The history goes in app-db under
  ::db/undo, which is editor-only, so it is never saved:

  {:undo [snapshot ...]   oldest first, so the last one is undone first
   :redo [snapshot ...]   likewise
   :last {:key k :time t} the last edit recorded, for coalescing}

  A snapshot is the document part of app-db before an edit (see
  `document-part`). The document is persistent data, so a snapshot shares
  almost all of itself with the document, and costs little to keep.

  What counts as an edit is narrower than what needs saving (see
  db/unsaved-change?): changes to the selection and the view state (map
  position, table filters, colouring) need saving, but undo leaves them
  alone. Saving changes nothing here; loading replaces the document, so it
  forgets the history."
  (:require [thermos-specs.candidate :as candidate]
            [thermos-specs.document :as document]
            [thermos-specs.view :as view]
            [thermos-frontend.db :as db]))

(def max-depth
  "How many edits can be undone"
  50)

(def coalesce-ms
  "Edits made by the same sort of event within this long of each other are
  undone together, see `coalesce-key`"
  1000)

(def ^:private kept-keys
  "Document keys which undo and redo leave as they are, and changes to
  which aren't edits"
  #{::view/view-state ::document/migration-messages})

(defn- document-key? [k]
  (not (or (document/is-transient-key? k) (contains? kept-keys k))))

(defn- document-part
  "The part of `db` which undo puts back: the document, without the
  editor-only keys (::db/server, the spatial index, the history itself)
  and the `kept-keys`"
  [db]
  (into {} (filter (comp document-key? key)) db))

(defn- restore
  "`db` with its document part replaced by `snapshot`"
  [db snapshot]
  (into (into {} (remove (comp document-key? key)) db) snapshot))

(defn history [db] (::db/undo db))

;; What counts as an edit

(defn- changed-candidates
  "The candidates which aren't identical in `old` and `new` (::candidates
  maps), as two maps of id to candidate, without their selection"
  [old new]
  (let [unselect #(dissoc % ::candidate/selected)
        new-changed (reduce-kv (fn [m id n]
                                 (let [o (get old id)]
                                   (cond-> m (not (identical? o n)) (assoc id n))))
                               {} new)
        old-changed (reduce-kv (fn [m id o]
                                 (cond-> m
                                   (or (contains? new-changed id) (not (contains? new id)))
                                   (assoc id o)))
                               {} old)]
    [(update-vals old-changed unselect)
     (update-vals new-changed unselect)]))

(defn edit?
  "Whether going from `old-db` to `new-db` is an edit which can be undone:
  one which changes the document in a way that would need saving, other
  than the selection or the `kept-keys`, and which isn't saving or loading
  (which increase the ::db/server :revision)."
  [old-db new-db]
  (and (not (identical? old-db new-db))
       (= (:revision (db/server old-db)) (:revision (db/server new-db)))
       (let [oc (::document/candidates old-db)
             nc (::document/candidates new-db)
             [oc nc] (if (identical? oc nc) [nil nil] (changed-candidates oc nc))
             ;; interesting-change? on just the changed candidates
             strip #(-> (apply dissoc %1 kept-keys) (assoc ::document/candidates %2))]
         (document/interesting-change? (strip old-db oc) (strip new-db nc)))))

(defn- barrier?
  "Whether going from `old-db` to `new-db` is a load, which replaces the
  document with the server's, whether that is the network as last saved
  or a run's solution"
  [old-db new-db]
  (and (not= (:revision (db/server old-db)) (:revision (db/server new-db)))
       (document/interesting-change? old-db new-db)))

;; Recording

(defn- coalesce-key
  "Consecutive edits by events with the same coalesce key (all but the
  last argument, so the path for :doc/set, or the diameter for
  :pipe/change-cost) are coalesced, so typing into an input makes one
  edit rather than one per key. Events with no arguments aren't."
  [event]
  (when (> (count event) 1) (pop event)))

(defn- push [stack snapshot]
  (let [stack (conj (or stack []) snapshot)]
    ;; vec rather than subvec, which would keep hold of what it drops
    (cond-> stack (> (count stack) max-depth) (-> rest vec))))

(defn record
  "`new-db`, the result of `event` on `old-db` at time `now` (in ms), with
  the history updated: forgotten if the event loaded the document, or with
  a snapshot of `old-db` to undo to if it was an edit, unless it carries on
  from the last edit"
  [old-db new-db event now]
  (cond
    (barrier? old-db new-db)
    (dissoc new-db ::db/undo)

    (edit? old-db new-db)
    (let [{:keys [last] :as h} (history new-db)
          k (coalesce-key event)
          coalesce (and k (= k (:key last)) (< (- now (:time last)) coalesce-ms))]
      (assoc new-db ::db/undo
             (cond-> (assoc h :redo nil :last {:key k :time now})
               (not coalesce) (update :undo push (document-part old-db)))))

    :else new-db))

;; Undoing and redoing

(defn can-undo? [db]
  (and (seq (:undo (history db))) (not (db/running-or-queued? db))))

(defn can-redo? [db]
  (and (seq (:redo (history db))) (not (db/running-or-queued? db))))

(defn- step
  "Move the document back (`from` :undo, `to` :redo) or forward (the
  other way) through the history. While a network is running or queued,
  the document is what is being optimised, so this does nothing."
  [db from to]
  (let [h (history db)
        stack (get h from)]
    (if (and (seq stack) (not (db/running-or-queued? db)))
      (-> (restore db (peek stack))
          (assoc ::db/undo (assoc h
                                  from (pop stack)
                                  to (push (get h to) (document-part db))
                                  ;; the next edit is a new one
                                  :last nil)))
      db)))

(defn undo [db] (step db :undo :redo))

(defn redo [db] (step db :redo :undo))

(ns thermos-ui.frontend.io
  (:require [goog.net.XhrIo :as xhr] ;; https://developers.google.com/closure/library/docs/xhrio
            [goog.net.XhrManager :as xhr-manager]
            [cljs.reader :refer [read-string]]
            [thermos-ui.urls :as urls]
            ))

(defn load-document
  "Load a document from the `url`, and call `handler` with it
  once deserialized"
  [org proj id handler]
  (xhr/send
   (urls/document org proj id)
   (fn [e]
     (->> (.-target e)
          (.getResponseText)
          (read-string)
          handler))))

(defn save-document
  "Save a `document` to the given `url`"
  [org proj document run? cb]
  (xhr/send
   (str (urls/document org proj)
        (if run? "?run=1" ""))
   (fn [e]
     (cb (.getResponseHeader (.. e -target) "X-Problem-ID")))

   "POST"
   (let [data (js/FormData.)
         doc (pr-str document)
         blob (js/Blob. #js [doc] #js {"type" "text/edn"})]
     (.append data "file" blob (str proj ".edn"))
     data)
   ))


(defn delete-problem
  "Delete all a problem's documents."
  [org problem callback]
  (xhr/send
   (urls/document org problem)
   callback
   "DELETE")
  )

(let [pool (goog.net.XhrManager.)]
  (defn request-geometry
    "Load geometry from the database for the tile at `x`, `y`, `z`, and
  call `handler` with what comes back."
    [x y z handler]
    ;; Sometimes it breaks if there is already an active request with the same id,
    ;; so abort any such request.
    (.abort pool (str x y z) true)
    (.send pool
           (str x y z)
           (urls/tile x y z)
           nil nil nil nil
           #(handler (.. % -target getResponseJson)))
    )

  (defn get-outstanding-request-ids
    []
    (js->clj (.getOutstandingRequestIds pool)))

  (defn abort-request
    [id]
    (.abort pool id true)))

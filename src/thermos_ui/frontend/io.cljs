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
  [org proj document]
  (xhr/send
   (urls/document org proj)
   (fn [e] (js/console.log e))
   "POST"
   (let [data (js/FormData.)
         doc (pr-str document)
         blob (js/Blob. #js [doc] #js {"type" "text/edn"})]
     (.append data "file" blob (str proj ".edn"))
     data)
   ))

(let [pool (goog.net.XhrManager.)]
  (defn request-geometry
    "Load geometry from the database for the tile at `x`, `y`, `z`, and
  call `handler` with what comes back."
    [x y z handler]
    (.send pool
           (str x y z)
           (urls/tile x y z)
           nil nil nil nil
           (fn [e]
             (println "Got tile" x y z)
             (handler (.. e -target getResponseJson))))
    ))

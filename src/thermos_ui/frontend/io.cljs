(ns thermos-ui.frontend.io
  (:require [goog.net.XhrIo :as xhr] ;; https://developers.google.com/closure/library/docs/xhrio
            [cljs.reader :refer [read-string]]))

(defn load-document
  "Load a document from the `url`, and call `handler` with it
  once deserialized"
  [url handler]
  (xhr/send url
            (fn [e]
              (->> (.-target e)
                   (.getResponseText)
                   (read-string)
                   handler))))


(defn save-document
  "Save a `document` to the given `url`"
  [url document]

  )

(defn request-geometry
  "Load geometry from the database for the tile at `x`, `y`, `z`, and
  call `handler` with what comes back."
  [x y z handler]

  (let [url (str "/map/candidates/" z "/" x "/" y "/")
        on-success
        (fn [e]
          (handler (.. e -target getResponseJson)))]
    (xhr/send url on-success)))

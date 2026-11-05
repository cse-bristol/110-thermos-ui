;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.routes.responses
  (:require [ring.util.response :as response]
            [cognitect.transit :as transit]
            [cheshire.core :as json]
            [hiccup2.core :as hiccup])
  (:import [java.io ByteArrayInputStream]))

(defn html [content]
  (-> (response/response content)
      (response/content-type "text/html")))

(defn json [content]
  (-> (cond-> content
        (or (map? content) (string? content))
        (json/generate-string))
      (response/response)
      (response/content-type "application/json; charset=utf-8")))

(defn transit-json [data]
  (let [stream (java.io.ByteArrayOutputStream. 1024)
        w (transit/writer stream :json)]
    (transit/write w data)
    (-> (response/response (.toString stream))
        (response/content-type "application/transit+json; charset=utf-8"))))

(def deleted {:status 204})

(defn fragment
  "A response containing some hiccup rendered without the page around it,
  for htmx to swap in."
  [& content]
  (-> (str (hiccup/html (seq content)))
      (html)
      (response/charset "UTF-8")))

(defonce ^:private file-deleter
  (java.util.concurrent.Executors/newSingleThreadScheduledExecutor
   (reify java.util.concurrent.ThreadFactory
     (newThread [_ r]
       (doto (Thread. ^Runnable r "Delete temporary files")
         (.setDaemon true))))))

(defn temporary-file
  "A response whose body is `file`, a temporary file which is deleted
  once it's been sent.

  http-kit reads a file body when it starts the response, or memory-maps
  it if it's 20MB or more, rather than holding it on the heap as it
  would an InputStream's content. Since we can't tell when it's done,
  the file is deleted ten minutes later; a mapped file can be deleted
  while it is still being sent."
  [^java.io.File file]
  (.schedule ^java.util.concurrent.ScheduledExecutorService file-deleter
             ^Runnable (fn [] (.delete file))
             10 java.util.concurrent.TimeUnit/MINUTES)
  (response/response file))

(defn htmx-request? [request]
  (= "true" (get-in request [:headers "hx-request"])))

(defn htmx-redirect
  "Redirect to url; for a htmx request this is done by htmx, so the whole page
  changes rather than the redirected-to page being swapped in."
  [request url]
  (if (htmx-request? request)
    (-> (response/response "")
        (response/header "HX-Redirect" url))
    (response/redirect url :see-other)))

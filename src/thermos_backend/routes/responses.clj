(ns thermos-backend.routes.responses
  (:require [ring.util.response :as response]
            [cognitect.transit :as transit]
            [cheshire.core :as json])
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

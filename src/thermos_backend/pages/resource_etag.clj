;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.pages.resource-etag
  (:require [ring.middleware.resource :as resource]
            [ring.util.request :as request]
            [ring.util.codec :as codec]
            [clojure.java.io :as io])
  
  (:import  [java.util.zip CRC32]))

(defonce resource-checksums (atom {}))

(defn- compute-checksum [response-body]
  (with-open [is (io/input-stream response-body)]
    (let [buffer-size (int 2048)
          ba (byte-array buffer-size)
          crc-32 (new CRC32)]
      (loop []
        (let [num-bytes-read (.read is ba 0 buffer-size)]
          (when-not (= num-bytes-read -1)
            (.update crc-32 ba 0 num-bytes-read)
            (recur))))
      (.getValue crc-32))))

(defn wrap-resources [handler]
  (fn [request]
    (if-let [response (resource/resource-request request "public")]
      (let [path (codec/url-decode (request/path-info request))
            checksum (or (@resource-checksums path)
                         (let [checksum
                               (-> request
                                   (assoc :method :get)
                                   (resource/resource-request "public")
                                   (:body)
                                   (compute-checksum))]
                           
                           (swap! resource-checksums assoc path checksum)
                           checksum))]
        (assoc-in response [:headers "ETag"]
                  (format "W/\"%s\""(str checksum))))
      (handler request))))


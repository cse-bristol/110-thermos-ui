;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.pages.cache-control
  (:require [ring.util.response :as response]))

(defn no-store
  "Update response for cache-control no-store. This will disable any caching."
  [resp]
  (response/header resp
                   "Cache-Control"
                   "no-store"))

(defn no-store? [resp]
  (-> resp
      (:headers)
      (get "Cache-Control")
      (= "no-store")))

(defn public
  "Update response for cache-control public /unless/ it is already private or no-store"
  [resp & {:keys [max-age] :or {max-age 86400}}]
  (update-in resp
             [:headers "Cache-Control"]

             #(cond
                (or (= "no-store" %)
                    (and % (.startsWith % "private")))
                %

                :else
                (format "public, max-age=%d" max-age))))

(defn private
  "Update response for cache-control private /unless/ it is already no-store"
  [resp & {:keys [max-age] :or {max-age 86400}}]
  (update-in resp
             [:headers "Cache-Control"]

             #(cond
                (= "no-store" %)
                %
                :else
                (format "private, max-age=%d" max-age))))

(defn etag [response etag]
  (assoc-in response [:headers "ETag"] etag))

(defn wrap-no-store [handler]
  (fn [request]
    (let [response (handler request)]
      (and response (no-store response)))))


(ns thermos-backend.pages.cache-control
  (:require [ring.util.response :as response]))

(defn no-store
  "Update response for cache-control no-store. This willl disable any caching."
  [resp]
  (response/header resp
                   "Cache-Control"
                   "no-store"))

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

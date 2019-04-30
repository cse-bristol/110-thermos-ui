(ns thermos-backend.pages.cache-control
  (:require [ring.util.response :as response]
            ))

(defn no-store [resp]
  (response/header resp
                   "Cache-Control"
                   "no-store"))

(defn public [resp & {:keys [max-age] :or {max-age 86400}}]
  (update-in resp
             [:headers "Cache-Control"]

             #(cond
                (or (= "no-store" %)
                    (and % (.startsWith % "private")))
                %

                :else
                (format "public, max-age=%d" max-age))))

(defn private [resp & {:keys [max-age] :or {max-age 86400}}]
  (update-in resp
             [:headers "Cache-Control"]

             #(cond
                (= "no-store" %)
                %
                :else
                (format "private, max-age=%d" max-age))))

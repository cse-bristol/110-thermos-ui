(ns thermos-backend.db.json-functions
  (:require [honeysql.core :as sql]))

(defn build-object [& kvs]
  {:pre [(even? (count kvs))]}
  (apply sql/call
   :json_build_object
   (map-indexed
    #(if (even? %1)
       (name %2)
       %2)
    kvs)))


(ns thermos-backend.util
  (:require [org.tobereplaced.nio.file :as nio]))

(defn create-temp-directory! [in-directory label]
  (let [wd (nio/path in-directory)]
    (.mkdirs (.toFile wd))
    (.toFile (nio/create-temp-directory! wd label))))


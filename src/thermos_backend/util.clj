(ns thermos-backend.util
  (:require [org.tobereplaced.nio.file :as nio]
            [clojure.java.io :as io]))

(defn create-temp-directory! [in-directory label]
  (let [wd (nio/path in-directory)]
    (.mkdirs (.toFile wd))
    (.toFile (nio/create-temp-directory! wd label))))

(defn remove-files! [& fs]
  (when-let [f (first fs)]
    (if-let [cs (seq (.listFiles (io/file f)))]
      (recur (concat cs fs))
      (do (io/delete-file f)
          (recur (rest fs))))))

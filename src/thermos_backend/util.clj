(ns thermos-backend.util
  (:require [org.tobereplaced.nio.file :as nio]
            [clojure.java.io :as io]
            [thermos-backend.config :refer [config]]
            [clojure.tools.logging :as log]
            [clojure.stacktrace]
            ))

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

(defn dump-error [ex msg & {:keys [type data]
                            :or {type "generic"}}]
  (try
    (if-let [dump-path (config :error-dumps)]
      (let [dump-dir (io/file dump-path (str type) (str (java.util.UUID/randomUUID)))]
        (.mkdirs dump-dir)
        (when data (spit (io/file dump-dir "data.edn") data))
        (with-open [w (io/writer (io/file dump-dir "trace.txt"))]
          (binding [*out* w]
            (println msg)
            (println)
            (clojure.stacktrace/print-throwable ex)
            (println)
            (clojure.stacktrace/print-cause-trace ex)))
        
        (log/error ex msg (.getCanonicalPath dump-dir)))
      (log/error ex msg))
    (catch Throwable ex
      (log/error ex "Error logging error" type msg))))

(comment
  (dump-error (ex-info "blarg" {}) "Wargarble"
              :type "whatsit"
              :data {:this :and :that :stuff}
              ))

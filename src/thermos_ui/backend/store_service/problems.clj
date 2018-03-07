(ns thermos-ui.backend.store-service.problems
   (:require [clojure.java.io :as io]
             [clojure.string :refer [join]]
             [clojure.string :as s]
             [environ.core :refer [env]]
             [digest]
             [clojure.edn :as edn]))

(defonce store-location (io/file (env :problem-store)))
(defonce file-ext ".edn")

(defn- remove-ext [name]
  (if (s/ends-with? name file-ext)
    (.substring name 0 (- (.length name)
                          (.length file-ext)))
    name))

(defn insert [org name temp-file]
  (let [hash (digest/md5 temp-file)
        store-file (io/file store-location org name (str hash file-ext))]
    (io/make-parents store-file)
    (io/copy temp-file store-file)

    {:org org
     :name name
     :id hash
     :date (.lastModified store-file)
     :file store-file}))

(defn ls [& parts]
  (let [top (apply io/file store-location parts)]
    (->> (file-seq top)
         (map (fn [path]
                (when (.isFile path)
                  (let [rel (str (.relativize (.toPath store-location)
                                              (.toPath path)))
                        [org name id] (s/split rel #"/")]
                    (when (and org name id)
                      ;; TODO remove suffix from id
                      {:org org
                       :name name
                       :id (remove-ext id)
                       :date (.lastModified path)
                       :file path}))
                  )))
         (filter identity)
         )))

(defn get-file [org name id]
  (let [out (io/file store-location org name (str id file-ext))]
    (when (.exists out) out)))

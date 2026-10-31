;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns build.less
  "Compile LESS with lessc (from nix). As with less4clj before, every
  `foo.main.less` under the source directory becomes `foo.css` at the
  same relative path under the target directory."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as string])
  (:import [java.nio.file FileSystems StandardWatchEventKinds WatchEvent$Kind]))

(defn- entry-points [source-dir]
  (->> (file-seq (io/file source-dir))
       (filter #(string/ends-with? (.getName %) ".main.less"))))

(defn- relative-path [dir file]
  (str (.relativize (.toPath (io/file dir)) (.toPath file))))

(defn build
  "Compile every entry point under `source-dir` into `target-dir`, with
  imports also searched for in `include-dirs`. Throws if anything fails
  to compile."
  [source-dir target-dir include-dirs]
  (doseq [in (entry-points source-dir)]
    (let [out (io/file target-dir (string/replace (relative-path source-dir in)
                                                  #"\.main\.less$" ".css"))
          _ (io/make-parents out)
          {:keys [exit err]}
          (try (apply sh "lessc"
                      (concat (for [d include-dirs] (str "--include-path=" d))
                              [(str in) (str out)]))
               (catch java.io.IOException e
                 (throw (ex-info "lessc is not on the PATH; run this inside `nix develop`" {} e))))]
      (when-not (zero? exit)
        (throw (ex-info (str "lessc failed for " in "\n" err) {:file (str in)}))))))

(defn watch
  "Build, then rebuild whenever a .less file under `source-dir` changes.
  Returns the watching thread."
  [source-dir target-dir include-dirs]
  (let [rebuild #(try (build source-dir target-dir include-dirs)
                      (println "Compiled LESS")
                      (catch Exception e (println (ex-message e))))
        service (.newWatchService (FileSystems/getDefault))
        kinds   (into-array WatchEvent$Kind [StandardWatchEventKinds/ENTRY_CREATE
                                             StandardWatchEventKinds/ENTRY_MODIFY
                                             StandardWatchEventKinds/ENTRY_DELETE])]
    (doseq [dir (file-seq (io/file source-dir)) :when (.isDirectory dir)]
      (.register (.toPath dir) service kinds))
    (rebuild)
    (doto (Thread.
           (fn []
             (loop []
               (let [key (.take service)
                     less? (some #(string/ends-with? (str (.context %)) ".less")
                                 (.pollEvents key))]
                 (.reset key)
                 (when less?
                   ;; editors write files in several steps
                   (Thread/sleep 100)
                   (rebuild))
                 (recur))))
           "less-watcher")
      (.setDaemon true)
      (.start))))

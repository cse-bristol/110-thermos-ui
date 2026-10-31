;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns build.dev
  "The development environment: `clojure -M:dev:server:client [--emacs]`.
  Starts an nREPL, the backend, shadow-cljs for the editor and a LESS watcher."
  (:require [mount.core :as mount]
            [clojure.java.io :as io]
            [clojure.tools.cli :refer [parse-opts]]
            [nrepl.server :as nrepl]
            [shadow.cljs.devtools.server :as shadow-server]
            [cider.piggieback]
            [shadow.cljs.devtools.server.nrepl :as shadow-nrepl]
            [shadow.cljs.devtools.api :as shadow]
            [build.less :as less]
            [build.vendor :as vendor]))

(def cli-options
  [["-e" "--emacs" "Use cider middleware for emacs"]])

(defonce repl-server (atom nil))

(defn- delete-tree [f]
  (when (.isDirectory f)
    (run! delete-tree (.listFiles f)))
  (io/delete-file f true))

(def ^:private generated (io/file "target/resources"))

(defn- clean-target
  "Empty target/, but keep target/resources itself. It is on the classpath,
  and the JVM only treats it as a directory if it existed at startup."
  []
  (doseq [f (.listFiles (io/file "target"))
          :when (not= f generated)]
    (delete-tree f))
  (run! delete-tree (.listFiles generated)))

(defn- start-shadow!
  "Start shadow-cljs's server, and have it watch and hot-reload the editor"
  []
  (shadow-server/start!)
  (shadow/watch :editor))

(defn start-dev [{:keys [emacs]}]
  (when-not (.isDirectory generated)
    (.mkdirs generated)
    (println "\n*** target/resources did not exist when the JVM started, so the backend")
    (println "*** will not be able to serve css and js. It exists now; please restart.\n"))

  (clean-target)

  (when-not (.isDirectory (io/file "node_modules"))
    (println "Installing npm packages")
    (-> (ProcessBuilder. ["npm" "ci"]) (.inheritIO) (.start) (.waitFor)))
  (vendor/copy-vendor-js "target/resources")

  (reset! repl-server
          (if emacs
            (do (require 'build.emacs)
                (nrepl/start-server :handler @(resolve 'build.emacs/emacs-middleware)))
            (nrepl/start-server
             :handler (nrepl/default-handler #'cider.piggieback/wrap-cljs-repl
                                             #'shadow-nrepl/middleware))))

  (let [port (:port @repl-server)
        port-file (io/file ".nrepl-port")]
    (println "Server-side nREPL port" port)
    (.deleteOnExit port-file)
    (spit port-file port))

  (try
    (require 'thermos-backend.core)
    ;; run webserver
    (mount/start)
    (catch Exception e
      (println "Unable to start back-end")
      (println "Continuing with rest of build")
      (.printStackTrace e)))

  (less/watch "resources" "target/resources" [vendor/less-include-dir])

  ;; cljs hot reloader, configured in shadow-cljs.edn. For a cljs REPL,
  ;; call (shadow.cljs.devtools.api/repl :editor) in the nREPL.
  (start-shadow!))

(defn -main [& args]
  (let [{:keys [options errors summary]} (parse-opts args cli-options)]
    (if errors
      (do (run! println errors)
          (println summary)
          (System/exit 1))
      (do (start-dev options)
          @(promise)))))

(in-ns 'user)

(require '[clojure.tools.namespace.repl :as tns])

;; this below is required because otherwise tools.namespace scans the
;; whole classpath, which unfortunately includes some clojure files that
;; refer to things which we do not have, making the refresh blow up.
(tns/set-refresh-dirs "src" "build-src")
(require '[mount.core :as mount])

(defn restart-backend
  "Call to force the backend to restart. Most of the time calling
  tns/refresh should be good enough."
  []
  (mount.core/stop)
  (require 'thermos-backend.core)
  (tns/refresh)
  (mount.core/start))

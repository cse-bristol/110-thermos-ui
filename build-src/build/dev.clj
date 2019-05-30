(require 'nrepl.server)
(require 'refactor-nrepl.middleware)
(require 'cider.nrepl)

(ns build.dev
  (:require [mount.core :as mount]
            [clojure.tools.namespace :as tns]
            [clojure.java.io :as io]
            [nrepl.server :as nrepl]
            [figwheel-sidecar.repl-api
              :as repl-api :refer [cljs-repl]]
            [less4clj.api :as less]
            [clojure.edn :as edn]))

;; start an nrepl to connect to

(defonce repl-server (atom nil))
(defn- require-and-resolve [sym]
  (require (symbol (namespace sym)))
  (resolve sym))

(defn start-dev [{nrepl-handler :nrepl-handler
                  debug-optimizations :debug-optimizations}]
  
  (reset! repl-server
          (nrepl/start-server :handler (require-and-resolve nrepl-handler)))
  
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

  ;; start cljs hot reloader
  (let [cljs-builds (edn/read-string (slurp "cljs-builds.edn"))]
    (repl-api/start-figwheel!
     {
      :figwheel-options {:server-logfile false}
      :build-ids (for [b cljs-builds] (str (:main b)))
      :all-builds
      (for [b cljs-builds]
        {:id (str (:main b))
         :figwheel true
         :source-paths ["src"]
         :compiler (cond-> b
                     debug-optimizations
                     (assoc :pretty-print true
                            :pseudo-names true)
                     )})
      }))

  ;; start less autobuilder
  (less/build
   {:source-paths ["resources"]
    :target-path "target/resources/"
    :auto true}))

(in-ns 'user)

(require '[clojure.tools.namespace.repl :as tns])

;; this below is required because otherwise tools.namespace scans the
;; whole classpath, which unfortunately includes some clojure files that
;; refer to things which we do not have, making the refresh blow up.
(tns/set-refresh-dirs "src" "build-src" "libs/thermos-importer/src")
(require '[mount.core :as mount])

(defn restart-backend
  "Call to force the backend to restart. Most of the time calling
  tns/refresh should be good enough."
  []
  (mount.core/stop)
  (require 'thermos-backend.core)
  (tns/refresh)
  (mount.core/start))


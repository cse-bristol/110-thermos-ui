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
(defn nrepl-handler []
  (require 'cider.nrepl)
  (ns-resolve 'cider.nrepl 'cider-nrepl-handler))

(defonce repl-server (atom nil))

(defn start-dev []
  (reset! repl-server (nrepl/start-server :handler (nrepl-handler)))
  (let [port (:port @repl-server)
        port-file (io/file ".nrepl-port")]
    (println "Server-side nREPL port" port)
    (.deleteOnExit port-file)
    (spit port-file port))

  (require 'thermos-backend.core)
  ;; run webserver
  (mount/start)

  ;; start cljs hot reloader
  (let [cljs-builds (edn/read-string (slurp "cljs-builds.edn"))]
    (repl-api/start-figwheel!
     {
      :build-ids (for [b cljs-builds] (str (:main b)))
      :all-builds
      (for [b cljs-builds]
        {:id (str (:main b))
         :figwheel true
         :source-paths ["src"]
         :compiler b})
      }))

  ;; start less autobuilder
  (less/build
   {:source-paths ["resources"]
    :target-path "target/resources/"
    :auto true}))


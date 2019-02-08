;; This is the dev script, which will start a dev server
;; You can run it with clj -Adev:server:client dev.clj

(require '[mount.core :as mount]
         '[clojure.tools.namespace :as tns]
         '[clojure.java.io :as io]
         '[thermos-backend.core]
         '[nrepl.server :as nrepl]
         '[figwheel-sidecar.repl-api
           :as repl-api :refer [cljs-repl]]
         '[less4clj.api :as less]
         '[clojure.edn :as edn])

;; start an nrepl to connect to
(defn nrepl-handler []
  (require 'cider.nrepl)
  (ns-resolve 'cider.nrepl 'cider-nrepl-handler))

(defonce repl-server (nrepl/start-server :handler (nrepl-handler)))

(let [port (:port repl-server)
      port-file (io/file ".nrepl-port")]
  (println "Server-side nREPL port" port)
  (.deleteOnExit port-file)
  (spit port-file port))

;; start the server-side component
(mount/start)

(def cljs-builds
  (edn/read-string (slurp "cljs-builds.edn")))

(repl-api/start-figwheel!
 {
  :build-ids (for [b cljs-builds] (str (:main b)))
  :all-builds
  (for [b cljs-builds]
    {:id (str (:main b))
     :figwheel true
     :source-paths ["src"]
     :compiler b})
  })

(less/build
 {:source-paths ["resources"]
  :target-path "target/resources/"
  :auto true})

;; start lesscss compiler to make the css
;; :figwheel-options
  ;; (when port
  ;;                     {:nrepl-port       (some-> port Long/parseLong)
  ;;                      :nrepl-middleware ["cider.nrepl/cider-middleware"
  ;;                                         "refactor-nrepl.middleware/wrap-refactor"
  ;;                                         "cemerick.piggieback/wrap-cljs-repl"]})

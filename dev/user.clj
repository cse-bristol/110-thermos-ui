(ns boot.user
  (:require [mount.core :as mount]
            [clojure.tools.namespace.repl :refer [refresh]]
            [thermos-backend.core]
            [thermos-backend.queue :as queue]
            [thermos-backend.solver.core :as solver]))

;; connect the repl and start things up by using this code -main is
;; the entrypoint in thermos-backend.core, but we can just do some stuff by hand:

(defn start []
  (mount/start)
  (queue/consume :problems solver/consume-problem))

(defn stop []
  (mount/stop))

(defn restart []
  (stop)
  (refresh) ;; this needs work to stop it refreshing this ns and
            ;; breaking things.
  (start))

;; or you can just refresh
(comment
  (refresh)
  )

;; To get a problem out of one thermos and into another:
;; 1. Run this javascript in the console
;; (function() { eval(document.head.getElementsByTagName('script')[0].text); var blob = new Blob([thermos_preloads], {type:'text/plain'}); var  u = URL.createObjectURL(blob); var a = document.createElement('a'); a.href = u; a.download = window.title + ".edn"; a.dispatchEvent(new MouseEvent("click")); })()
;; 2. Use the function below to load it into a project

(require '[clojure.java.io :as io])
(require '[clojure.edn :as edn])

(defn read-preload [out-path name project-id map-id]
  (let [preloads (with-open [r (java.io.PushbackReader. (io/reader out-path))]
                   (edn/read r))]
    (thermos-backend.db.projects/save-network!
     "tom.hinton@cse.org.uk" project-id map-id name (:initial-state preloads))))


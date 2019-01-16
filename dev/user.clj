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


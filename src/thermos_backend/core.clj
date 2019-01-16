(ns thermos-backend.main
  (:require [org.httpkit.server :as httpkit]
            [thermos-backend.config :refer [config]]
            [thermos-backend.handler :as handler]
            [thermos-backend.queue :as queue]
            [thermos-backend.solver.core :as solver]
            [clojure.tools.logging :as log]
            [mount.core :refer [defstate]]
            [mount.core :as mount])
  (:gen-class))

(defstate server
  :start
  (let [server-config
        {:max-body (* 1024 1024
                      (Integer/parseInt (config :web-server-max-body)))
         :no-cache (= "true" (config :web-server-disable-cache))
         :port (Integer/parseInt (config :web-server-port))}]
    (httpkit/run-server handler/all server-config))
  :stop
  (and server (server :timeout 100)))

(defn -main [& args]
  (log/info "Starting THERMOS application")
  (mount/start)
  (queue/consume :problems solver/consume-problem)
  ;; wait for stop.
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. #(mount/stop))))



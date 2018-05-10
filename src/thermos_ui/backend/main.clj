(ns thermos-ui.backend.main
  (:require [org.httpkit.server :as httpkit]
            [thermos-ui.backend.handler :refer [app]]
            [thermos-ui.backend.config :refer [config]]
            [thermos-ui.backend.queue :as queue]
            )
  (:gen-class))

(defn -main [& args]
  ;; we need to start the run-queue
  (queue/start-consumer-thread!)

  ;; we need to say that we are happy to solve problems
  ;; off the run queue

  ;; and we need to handle web requests
  (httpkit/run-server #'app
                      {:max-body (* 8388608 200)  ;; 800mega
                       :port (Integer/parseInt (config :server-port))
                       :join? true}))



(ns thermos-ui.backend.main
  (:require [org.httpkit.server :refer [run-server]]
            [thermos-ui.backend.handler :refer [app]]
            [thermos-ui.backend.config :refer [config]])
  (:gen-class))

(defn -main [& args]
  (run-server #'app {:max-body (* 8388608 200)  ;; 800mega
                     :port (Integer/parseInt (config :server-port)) :join? true}))



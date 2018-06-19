(ns thermos-ui.backend.main
  (:require [org.httpkit.server :as httpkit]
            [thermos-ui.backend.config :as config]
            [com.stuartsierra.component :as component]
            [thermos-ui.backend.db :refer [new-database]]
            [thermos-ui.backend.handler :as handler]
            [thermos-ui.backend.solver.core :refer [new-solver]]
            [thermos-ui.backend.queue :refer [new-queue]])
  (:gen-class))

(defrecord WebServer [port no-cache http-server database queue]
  component/Lifecycle

  (start [component]
    (assoc component :http-server
           (httpkit/run-server
            (handler/all no-cache (:database component) (:queue component))
            {:max-body (* 8388608 200)  ;; 800 megabytes
             :port (:port component)})))

  (stop [component]
    ((:http-server component) :timeout 100)
    (assoc component :http-server nil)))

(defn create-system []
  (let [config (config/load-values)]
    (println "Creating system...")
    
    (component/system-map
     :database (new-database config)

     :queue (component/using
             (new-queue config)
             [:database])

     :solver (component/using
              (new-solver config)
              [:queue :database])

     :webserver (component/using
                 (map->WebServer {:port (Integer/parseInt (config :server-port))
                                  :no-cache (= "true" (config :disable-cache))})
                 [:database :queue])
     ))
  )

(defn -main [& args]
  (component/start (create-system)))


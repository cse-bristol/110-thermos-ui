(ns thermos-backend.main
  (:require [org.httpkit.server :as httpkit]
            [thermos-backend.config :as config]
            [com.stuartsierra.component :as component]
            [thermos-backend.db :refer [new-database]]
            [thermos-backend.handler :as handler]
            [thermos-backend.solver.core :refer [new-solver]]
            [thermos-backend.queue :refer [new-queue]]

            [clojure.tools.logging :as log])
  (:gen-class))

(defrecord WebServer [port no-cache http-server database queue]
  component/Lifecycle

  (start [component]
    (assoc component :http-server
           (httpkit/run-server
            (handler/all no-cache (:database component) (:queue component))
            {:max-body (:max-body component)
             :port (:port component)})))

  (stop [component]
    ((:http-server component) :timeout 100)
    (assoc component :http-server nil)))

(defn create-system []
  (let [config (config/load-values)]
    (component/system-map
     :database (new-database config)

     :queue (component/using
             (new-queue config)
             [:database])

     :solver (component/using
              (new-solver config)
              [:queue :database])

     :webserver (component/using
                 (map->WebServer {:port (Integer/parseInt (config :web-server-port))
                                  :no-cache (= "true" (config :web-server-disable-cache))
                                  :max-body (* 1024 1024
                                               (Integer/parseInt (config :web-server-max-body)))
                                  })
                 [:database :queue])
     )))

(defn -main [& args]
  (log/info "Starting THERMOS application")
  (component/start (create-system)))


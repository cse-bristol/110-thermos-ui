(ns thermos-backend.core
  (:require [org.httpkit.server :as httpkit]
            [thermos-backend.config :refer [config]]
            [thermos-backend.handler :as handler]
            [thermos-backend.queue :as queue]
            [thermos-backend.solver.core :as solver]
            [clojure.tools.logging :as log]
            [compojure.core]
            [mount.core :refer [defstate]]
            [mount.core :as mount])
  (:gen-class))

;; this is horrible but is the easiest option for now

(in-ns 'compojure.core)


(defn ^:no-doc make-context [route make-handler]
  (letfn [(handler
            ([request]
             (when-let [context-handler (make-handler request)]
               (context-handler request)))
            ([request respond raise]
             (if-let [context-handler (make-handler request)]
               (context-handler request respond raise)
               (respond nil))))]
    (if (#{":__path-info" "/:__path-info"} (:source route))
      handler
      (fn
        ([request]
         (if-let [request (context-request request route)]
           (handler request)))
        ([request respond raise]
         (if-let [request (context-request request route)]
           (handler request respond raise)
           (respond nil)))))))

(in-ns 'thermos-backend.core)

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
  ;; wait for stop.
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. #(mount/stop))))



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

;; this is a horrible way to apply a fix for a bug but is the easiest
;; option for now, because the patched version of compojure is not
;; released and can't be used as a git dependency

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

(in-ns 'ring.util.response)

;; so this is monstrous; when we put our jar file in the nix store
;; it ends up with a modified date of the unix epoch
;; this is a hack to find really silly start dates and make them
;; program startup time instead.

(let [start-time (java.util.Date.)
      orig connection-last-modified]
  (defn- connection-last-modified [^java.net.URLConnection conn]
    (let [result (orig conn)]
      (if (< (.getTime result) 360000)
        start-time
        result))))

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



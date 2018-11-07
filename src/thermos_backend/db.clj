(ns thermos-backend.db
  (:require [ragtime.jdbc]
            [ragtime.repl]
            [jdbc.core :as jdbc]
            [hikari-cp.core :as hikari]
            [com.stuartsierra.component :as component]
            ))

(defrecord Database [host port dbname user password datasource]
  component/Lifecycle
  (start [component]
    (let [db-config {:dbtype   "postgresql"
                     :dbname   dbname
                     :host     host
                     :user     user
                     :password password}

          ragtime-config
          {:datastore (ragtime.jdbc/sql-database db-config)
           :migrations (ragtime.jdbc/load-resources "migrations")}

          datasource (hikari/make-datasource
                      {:connection-timeout 30000
                       :idle-timeout 600000
                       :max-lifetime 1800000
                       :minimum-idle 10
                       :maximum-pool-size  10
                       :adapter "postgresql"
                       :username (:user db-config)
                       :password (:password db-config)
                       :database-name (:dbname db-config)
                       :server-name (:host db-config)
                       :port-number 5432}
                      )]
      
      (ragtime.repl/migrate ragtime-config)
      ;; run migrations
      
      (assoc component :datasource datasource)))

  (stop [component]
    (hikari/close-datasource (:datasource component))
    (assoc component :datasource nil)))

(defn new-database [config]
  (map->Database {:host     (config :pg-host)
                  :user     (config :pg-user)
                  :password (config :pg-password)
                  :dbname   (config :pg-db)
                  }))

(defmacro with-connection
  "Run some computation with a connection open and bound, closing it afterwards."
  [[binding value] & compute]
  `(let [~binding (if (instance? Database ~value)
                    (jdbc/connection (:datasource ~value))
                    ~value)]
     (try
       ~@compute
       (finally
         (when (instance? Database ~value)
           (.close ~binding))))))



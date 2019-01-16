(ns thermos-backend.db
  (:require [ragtime.jdbc]
            [ragtime.repl]
            [jdbc.core :as jdbc]
            [hikari-cp.core :as hikari]
            [mount.core :refer [defstate]]
            [thermos-backend.config :refer [config]]))

(defstate conn
  :start
  (let [db-config {:dbtype   "postgresql"
                   :dbname   (config :pg-db)
                   :host     (config :pg-host)
                   :user     (config :pg-user)
                   :password (config :pg-password)}

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
                     :port-number 5432})]
    (ragtime.repl/migrate ragtime-config)
    datasource)
  :stop
  (hikari/close-datasource conn))

(defmacro with-connection
  "Run some computation with a connection open and bound, closing it afterwards."
  [[binding] & compute]
  `(with-open [~binding (jdbc/connection conn)]
     ~@compute))



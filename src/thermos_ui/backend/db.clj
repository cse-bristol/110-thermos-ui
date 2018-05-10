(ns thermos-ui.backend.db
  (:require [ragtime.jdbc]
            [ragtime.repl]
            [jdbc.core :as jdbc]
            [hikari-cp.core :as hikari]
            [thermos-ui.backend.config :refer [config]]))

(def database
  (future-call
   #(let [db-config {:dbtype   "postgresql"
                     :dbname   (config :pg-db-geometries)
                     :host     (config :pg-host)
                     :user     (config :pg-user)
                     :password (config :pg-password)}
          ragtime-config {:datastore (ragtime.jdbc/sql-database db-config)
                          :migrations (ragtime.jdbc/load-resources "migrations")}
          ]

      (ragtime.repl/migrate ragtime-config)

      (hikari/make-datasource
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
       )
      )))

(defn connection [] (jdbc/connection @database))


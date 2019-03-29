(ns thermos-backend.db
  (:require [ragtime.jdbc]
            [ragtime.repl]
            [jdbc.core :as jdbc]
            [jdbc.proto :as proto]
            [clojure.tools.logging :as log]
            [honeysql.core :as sql]
            [hikari-cp.core :as hikari]
            [mount.core :refer [defstate]]
            [thermos-backend.config :refer [config]]
            [honeysql.helpers :as h]
            [honeysql-postgres.helpers :as p]
            [clojure.string :as string]
            [clojure.data.json :as json])
  
  (:import [org.postgresql.util PGobject]))

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

(defn- normalize-column-name ^String [^String c]
  (-> c (string/lower-case) (.replace \_ \-)))

(def ignore-close
  (reify
    java.io.Closeable
    (close [_])))

(defmacro or-connection
  "Run some computation with a connection open and bound, closing it afterwards.
  If the connection is already there, just run the computation"
  {:style/indent :defn}
  [[binding] & compute]
  `(with-open [maybe-nothing#
               ^java.io.Closeable
               (if ~binding ignore-close (jdbc/connection conn))]
     (let [~binding (or ~binding maybe-nothing#)]
       ~@compute)))

(defmacro with-connection
  "Run some computation with a connection open and bound, closing it afterwards."
  [[binding] & compute]
  `(with-open [~binding ^java.io.Closeable (jdbc/connection conn)]
     ~@compute))

(defn execute!
  "Run a query for side-effects which returns no results.
  If the query is a map, it's formatted with `sql/format`. If you
  supply a connection, the query is run in that connection. Analogous
  to `fetch!`."
  ([query]
   (with-connection [conn] (execute! query conn)))
  
  ([query conn]
   (let [query (if (map? query) (sql/format query) query)]
     (try
       (jdbc/execute conn query)
       (catch Exception e
         (log/error e "Exception executing query: " query)
         (throw e))))))

(defn fetch-one!
  ([query]
   (with-connection [conn] (fetch-one! query conn)))
  ([query conn]
   (let [query (if (map? query) (sql/format query) query)]
     (try
       (jdbc/fetch-one
        conn query
        {:identifiers normalize-column-name})
       (catch Exception e
         (log/error e "Exception executing query: " query)
         (throw e))))))

(defn fetch!
  "Run a query which returns some results.
  If the query is a map, it's put through `sql/format`. Optionally you
  can supply a database connection as the 2nd argument, which you
  could acquire using `with-connection`, which see. This is useful if
  you are doing many database operations in a row or want to use
  `jdbc/atomic`"
  ([query]
   (with-connection [conn] (fetch! query conn)))
  ([query conn]
   (let [query (if (map? query) (sql/format query) query)]
     (try
       (jdbc/fetch
        conn query
        {:identifiers normalize-column-name})
       (catch Exception e
         (log/error e "Exception executing query: " query)
         (throw e))))))

(defn insert-one!
  ([table record]
   (with-connection [conn] (insert-one! table record conn)))
  ([table record conn]
   {:pre [(keyword? table) (map? record)]}
   (-> (h/insert-into table)
       (h/values [record])
       (p/returning :id)
       (fetch!)
       (first)
       (:id))))

;; This should allow us to read json results transparently from the
;; database. It's useful in concert with the functions to_json and
;; json_agg etc, effectively returning subquery hierarchy in one go.
(extend-protocol proto/ISQLResultSetReadColumn
  PGobject
  (from-sql-type [pgobj conn metadata index]
    (cond-> (.getValue pgobj)
      (= "json" (.getType pgobj))
      (json/read-str :key-fn #(keyword (normalize-column-name %))))))


;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.db.migration
  (:require [jdbc.core :as jdbc]
            [resauce.core :as resauce]
            [clojure.string :as string]
            [honeysql.helpers :as h]
            [honeysql.core :as sql]
            [clojure.set :as set]
            [clojure.tools.logging :as log]))

;; this is my alternative to ragtime migrations
;; downsides - stores more stuff in the DB, nonreversible
;; upsides - can append to files with migrations, does migrations entirely in an atomic block so doesn't break everything

(defn- load-migration-files [migration-files]
  (->> migration-files
       (map (fn [s]
              (if (string? s)
                (java.net.URL. s)
                s)))
       
       (filter #(.endsWith (.getPath %) ".sql"))
       (mapcat (fn [u]
                 (let [file (last (string/split (.getPath u) #"/"))]
                   (for [[row sql] (map-indexed vector (string/split (slurp u) #"(?m)^--;;.*$"))]
                     {:sql (string/trim sql) :row row :file file}))))
       (sort-by (juxt :file :row))))

(defn force
  "For manual use only - tell the database that you have definitely
  applied all the migrations and it is in a correct state."
  [conn & {:keys [migration-table migration-files]
           :or {migration-table "migrations"
                migration-files (resauce/resource-dir "migrations")}}]
  (jdbc/atomic conn
    (jdbc/execute conn "DELETE FROM migrations")
    (-> (h/insert-into (keyword migration-table))
        (h/values (load-migration-files migration-files))
        (sql/format)
        (->> (jdbc/execute conn)))))

(defn migrate [conn & {:keys [migration-table migration-files]
                       :or {migration-table "migrations"
                            migration-files (resauce/resource-dir "migrations")}}]
  
  (jdbc/atomic conn
    (jdbc/execute
     conn
     (format "CREATE TABLE IF NOT EXISTS %s (file text not null, row int not null, sql text not null);" migration-table))

    (jdbc/execute
     conn
     (format "UPDATE %s SET sql = trim(both ' \n' from sql);" migration-table))
    
    (let [migration-state (->> (jdbc/fetch conn (str "SELECT * FROM " migration-table))
                               (sort-by (juxt :file :row)))

          migration-contents (load-migration-files migration-files)]

      (let [[historic remaining] (split-at (count migration-state) migration-contents)]
        (when-not (= historic migration-state)
          (throw
           (ex-info "The database has had migrations applied to it from an alternate history"
                    {:unexpected-migrations (set/difference (set migration-state) (set historic))
                     :lost-migrations (set/difference (set historic) (set migration-state))})))

        ;; otherwise we are good
        (doseq [migration remaining]
          (log/info "Run migration: "
                    (update migration :sql #(first (.split % "\n"2))))
          (if (= \! (first (:sql migration)))
            (let [migration-function (symbol (.substring (:sql migration) 1))]
              (require (symbol (namespace migration-function)))
              ((resolve migration-function) conn))
            
            (try (jdbc/execute conn (:sql migration))
                 (catch Exception e
                   (log/error "Migration failed!\n" (:sql migration) e)
                   (throw
                    (ex-info (str "Error running migration: " (.getMessage e))
                             migration e)))))
          )
        
        (when (seq remaining)
          (-> (h/insert-into (keyword migration-table))
              (h/values remaining)
              (sql/format)
              (->> (jdbc/execute conn))))))))



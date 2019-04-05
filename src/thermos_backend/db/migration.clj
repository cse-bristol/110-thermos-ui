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

(defn migrate [conn & {:keys [migration-table migration-files]
                       :or {migration-table "migrations"
                            migration-files (resauce/resource-dir "migrations")}}]
  (jdbc/atomic conn
    (jdbc/execute
     conn
     "CREATE TABLE IF NOT EXISTS migrations (file text not null, row int not null, sql text not null);")

    (let [migration-state (->> (jdbc/fetch conn (str "SELECT * FROM " migration-table))
                               (sort-by (juxt :file :row)))

          migration-contents (->> migration-files
                                  (filter #(.endsWith (.getPath %) ".sql"))
                                  (mapcat (fn [u]
                                            (let [file (last (string/split (.getPath u) #"/"))]
                                              (for [[row sql] (map-indexed vector (string/split (slurp u) #"(?m)^--;;.*$"))]
                                                {:sql sql :row row :file file}))))
                                  (sort-by (juxt :file :row)))]


      (let [[historic remaining] (split-at (count migration-state) migration-contents)]
        (when-not (= historic migration-state)
          (throw
           (ex-info "The database has had migrations applied to it from an alternate history"
                    {:unexpected-migrations (set/difference migration-state historic)
                     :lost-migrations (set/difference historic migration-state)})))

        ;; otherwise we are good
        (doseq [migration remaining]
          (log/info "Run migration: " (dissoc migration :sql))
          (try (jdbc/execute conn (:sql migration))
               (catch Exception e
                 (log/error "Migration failed!\n" (:sql migration) e)
                 (throw
                  (ex-info (str "Error running migration: " (.getMessage e))
                           migration e)))))
        
        (when (seq remaining)
          (-> (h/insert-into (keyword migration-table))
              (h/values remaining)
              (sql/format)
              (->> (jdbc/execute conn))))))))



(ns thermos-ui.backend.problems.db
  (:require [thermos-ui.backend.db :as db]
            [jdbc.core :as jdbc]
            [honeysql.core :as sql]
            [honeysql.helpers :refer :all]
            [honeysql-postgres.format :refer :all]
            [honeysql-postgres.helpers :refer :all]
            ))

(defn insert! [db org name temp-file]
  (let [content (slurp temp-file)]
    (db/with-connection [conn db]
      (-> (insert-into :problems)
          (values [{:org org :name name :content content}])
          (returning :id)
          (sql/format)
          (->> (jdbc/fetch conn))
          (first)
          :id))))

(defn- ls* [db restriction]
  (db/with-connection [conn db]
    (-> (select :id :org :name :created :has_run)
        (from :problems)
        restriction
        (sql/format)
        (->> (jdbc/fetch conn)
             (map #(clojure.core/update % :created (fn [t] (.getTime t)))))
        )))

(defn ls
  ([db] (ls* db identity))
  ([db org] (ls* db #(where % [:= :org org])))
  ([db org name] (ls* db #(where % [:and [:= :org org] [:= :name name]]))))

(defn delete! [db org name]
  (db/with-connection [conn db]
    (-> (delete-from :problems)
        (where [:and [:= :org org] [:= :name name]])
        (sql/format)
        (->> (jdbc/execute conn)))))

(defn get-content [db org name id]
  (db/with-connection [conn db]
    (->
     (select :content)
     (from :problems)
     (where [:and [:= :org org] [:= :name name] [:= :id id]])
     (sql/format)
     (->> (jdbc/fetch conn))
     (first)
     :content)))

(defn get-details [db org name id]
  (db/with-connection [conn db]
    (->
     (select :content :job :has_run)
     (from :problems)
     (where [:and [:= :org org] [:= :name name] [:= :id id]])
     (sql/format)
     (->> (jdbc/fetch conn))
     (first))))

(defn add-solution [db org name id result]
  (db/with-connection [conn db]
    (-> (update :problems)
        (sset {:content result :has_run true})
        (where [:and [:= :org org] [:= :name name] [:= :id id]])
        (sql/format)
        (->> (jdbc/execute conn)))))

(defn set-job-id [db org name id job-id]
  (db/with-connection [conn db]
    (-> (update :problems)
        (sset {:job job-id})
        (where [:and [:= :org org] [:= :name name] [:= :id id]])
        (sql/format)
        (->> (jdbc/execute conn)))))

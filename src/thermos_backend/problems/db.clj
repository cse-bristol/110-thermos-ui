(ns thermos-backend.problems.db
  (:require [thermos-backend.db :as db]
            [jdbc.core :as jdbc]
            [honeysql.core :as sql]
            [honeysql.helpers :refer :all]
            [honeysql-postgres.format :refer :all]
            [honeysql-postgres.helpers :refer :all]
            ))

(defn insert! [org name temp-file]
  (let [content (slurp temp-file)]
    (db/with-connection [conn]
      (-> (insert-into :problems)
          (values [{:org org :name name :content content}])
          (returning :id)
          (sql/format)
          (->> (jdbc/fetch conn))
          (first)
          :id))))

(defn- ls* [restriction]
  (db/with-connection [conn]
    (-> (select :id :org :name :created :has_run)
        (from :problems)
        restriction
        (sql/format)
        (->> (jdbc/fetch conn)
             (map #(clojure.core/update % :created (fn [t] (.getTime t)))))
        )))

(defn ls
  ([] (ls* identity))
  ([org] (ls* #(where % [:= :org org])))
  ([org name] (ls* #(where % [:and [:= :org org] [:= :name name]]))))

(defn delete! [org name]
  (db/with-connection [conn]
    (-> (delete-from :problems)
        (where [:and [:= :org org] [:= :name name]])
        (sql/format)
        (->> (jdbc/execute conn)))))

(defn get-content [org name id]
  (db/with-connection [conn]
    (->
     (select :content)
     (from :problems)
     (where [:and [:= :org org] [:= :name name] [:= :id id]])
     (sql/format)
     (->> (jdbc/fetch conn))
     (first)
     :content)))

(defn get-details [org name id]
  (db/with-connection [conn]
    (->
     (select :content :job :has_run)
     (from :problems)
     (where [:and [:= :org org] [:= :name name] [:= :id id]])
     (sql/format)
     (->> (jdbc/fetch conn))
     (first))))

(defn add-solution [org name id result]
  (db/with-connection [conn]
    (-> (update :problems)
        (sset {:content result :has_run true})
        (where [:and [:= :org org] [:= :name name] [:= :id id]])
        (sql/format)
        (->> (jdbc/execute conn)))))

(defn set-job-id [org name id job-id]
  (db/with-connection [conn]
    (-> (update :problems)
        (sset {:job job-id})
        (where [:and [:= :org org] [:= :name name] [:= :id id]])
        (sql/format)
        (->> (jdbc/execute conn)))))

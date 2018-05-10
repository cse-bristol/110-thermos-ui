(ns thermos-ui.backend.problems.db
  (:require [thermos-ui.backend.db :as db]
            [jdbc.core :as jdbc]
            [honeysql.core :as sql]
            [honeysql.helpers :refer :all]
            [honeysql-postgres.format :refer :all]
            [honeysql-postgres.helpers :refer :all]
            ))

(defn insert! [org name temp-file]
  (let [content (slurp temp-file)]
    (with-open [conn (db/connection)]
      (-> (insert-into :problems)
          (values [{:org org :name name :content content}])
          (returning :id)
          (sql/format)
          (->> (jdbc/fetch conn))
          (first)
          :id))))

(defn- ls* [restriction]
  (with-open [conn (db/connection)]
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
  (with-open [conn (db/connection)]
    (-> (delete-from :problems)
        (where [:and [:= :org org] [:= :name name]])
        (sql/format)
        (->> (jdbc/execute conn)))))

(defn get-content [org name id]
  (with-open [conn (db/connection)]
    (->
     (select :content)
     (from :problems)
     (where [:and [:= :org org] [:= :name name] [:= :id id]])
     (sql/format)
     (->> (jdbc/fetch conn))
     (first)
     :content)))

(defn add-solution [org name id result]
  (with-open [conn (db/connection)]
    (-> (update :problems)
        (sset {:content result :has_run true})
        (where [:and [:= :org org] [:= :name name] [:= :id id]])
        (sql/format)
        (->> (jdbc/execute conn)))))

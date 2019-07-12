(ns thermos-backend.content-migrations.core
  (:require [thermos-backend.db :as db]
            [honeysql.helpers :as h]
            [clojure.edn :as edn]))

(defn modify-networks-with [conn modifier]
  (let [network-ids (-> (h/select :id)
                        (h/from :networks)
                        (db/fetch! conn)
                        (->> (map :id)))]
    (doseq [id network-ids]
      (let [network-content
            (-> (h/select :content)
                (h/from :networks)
                (h/where [:= :id id])
                (db/fetch-one! conn)
                (:content)
                (edn/read-string)
                (modifier))]
        (-> (h/update :networks)
            (h/sset {:content (pr-str network-content)})
            (h/where [:= :id id])
            (db/execute! conn))))))

(ns thermos-backend.content-migrations.network-metadata
  (:require [thermos-backend.db :as db]
            [thermos-backend.db.network-metadata :refer [summarise]]
            [honeysql.helpers :as h]
            [honeysql.types :as sql-types]
            [clojure.edn :as edn]
            [clojure.tools.logging :as log]
            [cheshire.core :as json]))

(defn migrate [conn]
  (log/info "Populating network metadata...")
  (let [network-ids
        (-> (h/select :id)
            (h/from :networks)
            (db/fetch! conn)
            (->> (map :id)))]
    (doseq [id network-ids]
      (try (let [metadata
                 (-> (h/select :content)
                     (h/from :networks)
                     (h/where [:= :id id])
                     (db/fetch-one! conn)
                     (:content)
                     (edn/read-string)
                     (summarise))]
             (-> (h/update :networks)
                 (h/sset {:meta
                          (sql-types/call
                           :cast
                           (json/encode metadata)
                           :json)})
                 (h/where [:= :id id])
                 (db/execute! conn)))
           (catch Exception e
             (log/warn e "Unable to update metadata for" id))))))

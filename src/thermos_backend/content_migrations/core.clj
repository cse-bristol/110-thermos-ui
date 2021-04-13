;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.content-migrations.core
  (:require [thermos-backend.db :as db]
            [honeysql.helpers :as h]
            [clojure.edn :as edn]))

(defn modify-network-with [conn modifier id]
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
        (db/execute! conn))))

(defn modify-networks-with [conn modifier]
  (let [network-ids (-> (h/select :id)
                        (h/from :networks)
                        (db/fetch! conn)
                        (->> (map :id)))]
    (doseq [id network-ids]
      (modify-network-with conn modifier id))))


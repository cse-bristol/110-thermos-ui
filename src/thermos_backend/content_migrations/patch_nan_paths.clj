(ns thermos-backend.content-migrations.patch-nan-paths
  (:require [thermos-specs.document :as document]
            [thermos-specs.path :as path]
            [thermos-specs.candidate :as candidate]
            [thermos-backend.content-migrations.core :refer [modify-network-with]]
            [cljts.core :as jts]
            [clojure.tools.logging :as log]
            [thermos-backend.db :as db]
            [honeysql.helpers :as h]))

(defn- patch-nan-paths [doc]
  (document/map-candidates
   doc
   (fn [candidate]
     (cond-> candidate
       (and (candidate/is-path? candidate)
            (Double/isNaN (::path/length candidate)))
       (assoc ::path/length
              ;; geometry reader required
              (jts/geodesic-length
               (jts/json->geom (::candidate/geometry candidate))))))))

(defn migrate [conn]
  (let [bad-ids (-> (h/select :id)
                    (h/from :networks)
                    (h/where [:like :content "%##NaN%"])
                    (db/fetch! conn))]
    (log/info "Patching NaN length paths in" (count bad-ids))
    (doseq [id bad-ids]
      (modify-network-with conn patch-nan-paths id))))


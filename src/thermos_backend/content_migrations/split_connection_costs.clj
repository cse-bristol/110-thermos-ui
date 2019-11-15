(ns thermos-backend.content-migrations.split-connection-costs
  (:require [clojure.tools.logging :as log]
            [thermos-backend.content-migrations.core :refer [modify-networks-with]]
            [thermos-specs.document :as document]
            [thermos-specs.tariff :as tariff]
            [thermos-util :as util])) 

(defn- split-connection-costs [document]
  (let [tariffs (::document/tariffs document)
        having-ccs (filter
                    #(or (::tariff/fixed-connection-cost %)
                         (::tariff/variable-connection-cost %))
                    (vals tariffs))

        existing-ccs (::document/connection-costs document)
        ;; probably empty

        max-cc-id  (inc (reduce max -1 (keys existing-ccs)))
        ;; probably zero

        ;; pull the useful bits out of the old tariffs,
        ;; to make the new ccs
        new-ccs (map-indexed
                 (fn [i t]
                   (-> t
                       (select-keys
                        [::tariff/id
                         ::tariff/name
                         ::tariff/fixed-connection-cost
                         ::tariff/variable-connection-cost])
                       (assoc
                        ::tariff/cc-id
                        (+ i max-cc-id))))
                 having-ccs)

        ;; make a map relating tariff ID to new CC id
        cc-lookup
        (->> (for [t new-ccs]
               [(::tariff/id t) (::tariff/cc-id t)])
             (into {}))

        ;; and throw away the tariff id on the new ccs
        new-cc-map
        (util/assoc-by (for [x new-ccs]
                         (dissoc x ::tariff/id))
                       ::tariff/cc-id)
        
        ]

    (-> document
        ;; strip the old parameters out of tariffs
        (update ::document/tariffs
                #(->
                  (for [[k v] %]
                    [k
                     (dissoc v
                             ::tariff/fixed-connection-cost
                             ::tariff/variable-connection-cost)])
                  (->> (into {}))))

        ;; add on the new connection cost objects
        (update ::document/connection-costs
                merge
                new-cc-map)

        ;; update all the candidates so that they have the
        ;; connection cost definition that went with their tariff
        (document/map-candidates
         (fn [c]
           (let [cc-id (get cc-lookup
                            (::tariff/id c))]
             (cond-> c
               cc-id (assoc ::tariff/cc-id cc-id))))))))

(defn migrate [conn]
  (log/info "Separating connection costs from tariffs")

  (modify-networks-with conn split-connection-costs))

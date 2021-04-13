;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.content-migrations.patch-tariffs
  (:require [thermos-specs.document :as document]
            [thermos-specs.demand :as demand]
            [thermos-specs.tariff :as tariff]
            [clojure.test :as test]
            [thermos-util :refer [assoc-by]]
            [thermos-backend.content-migrations.core :refer [modify-networks-with]]
            [clojure.tools.logging :as log]))

(defn- infer-tariffs
  "We made a change to take fuel prices & connection costs off buildings
  and to move them into tariffs. This function should take a document
  where the costs & prices are on buildings, and convert them to use
  tariffs."
  {:test #(test/is
           (= (infer-tariffs
               {::document/candidates {1 {::demand/price 3 ::demand/connection-cost 4}
                                       2 {::demand/connection-cost 7}
                                       3 {::demand/price 3 ::demand/connection-cost 4}
                                       4 {::demand/connection-cost 7 ::demand/price 77}}
                ::demand/price 77})

              {::document/candidates {1 {::tariff/id 0}
                                      2 {::tariff/id 1}
                                      3 {::tariff/id 0}
                                      4 {::tariff/id 1}}
               ::document/tariffs {0 {::tariff/id 0 ::tariff/name "Tariff 0"
                                      ::tariff/standing-charge 0
                                      ::tariff/capacity-charge 0
                                      ::tariff/unit-charge 3
                                      ::tariff/fixed-connection-cost 0
                                      ::tariff/variable-connection-cost 4}
                                   1 {::tariff/id 1 ::tariff/name "Tariff 1"
                                      ::tariff/standing-charge 0
                                      ::tariff/capacity-charge 0
                                      ::tariff/unit-charge 77
                                      ::tariff/fixed-connection-cost 0
                                      ::tariff/variable-connection-cost 7}
                                   }}
              ))}
  [document]

  (let [old-default-price (::demand/price document 0)
        get-old-state (juxt #(::demand/price % old-default-price)
                            ::demand/connection-cost)
        old-combinations (set (map
                               get-old-state
                               (vals (::document/candidates document))))
        new-tariffs
        (into {}
              (map-indexed
               (fn [i [price con-cost]]
                 [[price con-cost]
                  {::tariff/id i
                   ::tariff/name (str "Tariff " i)
                   ::tariff/standing-charge 0
                   ::tariff/unit-charge (or price old-default-price)
                   ::tariff/capacity-charge 0

                   ::tariff/fixed-connection-cost 0
                   ::tariff/variable-connection-cost (or con-cost 0)}])
               old-combinations))

        new-tariff-map (assoc-by
                        (vals new-tariffs)
                        ::tariff/id)

        get-new-tariff-id
        (comp ::tariff/id new-tariffs get-old-state)]
    
    (-> document
        (assoc ::document/tariffs new-tariff-map)
        (dissoc ::demand/price)
        (document/map-candidates
         (fn [candidate]
           (-> candidate
               (assoc ::tariff/id
                      (get-new-tariff-id candidate))
               (dissoc ::demand/price
                       ::demand/connection-cost)))))))

(defn migrate [conn]
  (log/info "Extracting tariff definitions...")
  (modify-networks-with conn infer-tariffs)
  )

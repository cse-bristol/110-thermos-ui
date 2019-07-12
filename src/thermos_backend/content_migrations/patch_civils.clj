(ns thermos-backend.content-migrations.patch-civils
  (:require [thermos-specs.document :as document]
            [thermos-specs.path :as path]
            [thermos-specs.candidate :as candidate]
            [thermos-util :refer [assoc-by to-fixed]]
            [thermos-backend.content-migrations.core :refer [modify-networks-with]]
            [clojure.tools.logging :as log]))

(defn- infer-civils [document]
  (let [get-old-state
        (juxt #(::path/cost-per-m % 0)
              #(::path/cost-per-m2 % 0))
        
        civil-combos
        (-> document
            (::document/candidates)
            (vals)
            (->> (map get-old-state))
            (set))

        new-civils
        (->> civil-combos
             (map-indexed (fn [i [fixed variable]]
                       [[fixed variable]
                        {::path/civil-cost-id i
                         ::path/civil-cost-name
                         (if (and (zero? fixed)
                                  (zero? variable))
                           "No cost"
                           (str
                            (to-fixed fixed 0)
                            "/m + "
                            (to-fixed variable 0)
                            "/~m2"))
                         ::path/fixed-cost fixed
                         ::path/variable-cost variable}]))
             (into {}))
        ]
    (-> document

        ;; fix the candidates
        (document/map-candidates
         (fn [p]
           (if (candidate/is-path? p)
             (let [st (get-old-state p)
                   n (get new-civils st)]
               (-> p
                   (assoc ::path/civil-cost-id (::path/civil-cost-id n))
                   (dissoc ::path/cost-per-m ::path/cost-per-m2)))
             p)))

        (assoc ::document/civil-costs
               (-> new-civils
                   (vals)
                   (assoc-by ::path/civil-cost-id))))))

(defn migrate [conn]
  (log/info "Extracting civil cost definitions...")

  (modify-networks-with conn infer-civils))

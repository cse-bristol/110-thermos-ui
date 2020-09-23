(ns thermos-backend.content-migrations.tabulate-pipe-parameters
  "A migration to convert pipe parameters from the equational form to a straight table, and to add steam-related parameters into a problem."

  (:require [thermos-specs.document :as document]
            [thermos-specs.path :as path]))

(defn- generate-costs-for-old-parameters
  "In an earlier version, thermos represented pipe costs using a pair of functions
  of the form a + (b*dia)^k, one for pipe and one for dig cost

  This function is to generate a ::document/pipe-costs table from these parameters
  "
  [doc]

  (let [old-civils (::document/civil-costs doc)
        old-mech-c (::document/mechanical-cost-per-m doc)
        old-mech-m (::document/mechanical-cost-per-m2 doc)
        old-mech-e (::document/mechanical-cost-exponent doc)

        old-civ-e (::document/mechanical-cost-exponent doc)

        max-dia (min (::document/maximum-pipe-diameter doc 1.0) 1.0)
        min-dia (::document/minimum-pipe-diameter doc 0.01)

        dia-range (conj
                   (into
                    (sorted-set
                     20 25 32 40 50 65 80 100 125 150 200 250 300 400 450 500)
                    (range 600 (* 1000.0 max-dia) 100))
                   (* max-dia 1000.0)
                   (* min-dia 1000.0))
        ]
    
    ;; introduce nominal dia range

    {:rows
     (into {}
           (for [dia dia-range
                 :let [dia-m (/ dia 1000.0)]]
             [dia
              (reduce-kv
               (fn [a civil-id params]
                 (assoc a
                        civil-id
                        (Math/round
                         (+ (::path/fixed-cost params 0)
                            (Math/pow (* (::path/variable-cost params 0)
                                         dia-m)
                                      old-civ-e)))))
               {:pipe (Math/round (+ old-mech-c (Math/pow (* old-mech-m dia-m) old-mech-e)))}
               old-civils)]
             ))
     
     :civils
     (into {}
           (for [[id civ] old-civils] [id (::path/civil-cost-name
                                           civ
                                           (str "Civil cost " id))]))
     
     }
    ))

(defn tabulate-pipe-parameters [document]
  (cond-> document
    (not (contains? document ::document/pipe-costs))
    (-> 
     (assoc ::document/pipe-costs (generate-costs-for-old-parameters document))
     (dissoc ::document/civil-costs
             ::document/civil-cost-exponent
             ::document/mechanical-cost-exponent
             ::document/mechanical-cost-per-m
             ::document/mechanical-cost-per-m2
             ::document/maximum-pipe-diameter
             ::document/minimum-pipe-diameter)

     (assoc ::document/steam-pressure 1.6
            ::document/steam-velocity 20.0
            ::document/medium :hot-water)

     (update ::document/migration-messages
             conj
             :update-pipe-parameters
             )

     )))

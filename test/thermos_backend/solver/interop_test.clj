;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.solver.interop-test
  (:require [thermos-backend.solver.interop :as sut]
            [clojure.test :as t]

            [thermos-specs.document :as document]
            [thermos-specs.supply :as supply]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [hnzp-utils.finance :as finance]))

(defn- dp4 [n] (/ (Math/round (* n 1000.0)) 1000.0))



(comment
  (let [dom-ashp
        {:name "Dom ASHP",
         :fuel "Domestic Electricity"
         
         :capex-fixed           5001.66
         :capex-per-kwp         232.36
         :capex-per-m2          36.68

         :opex-fixed            180.0

         :repex-fixed           3397.63
         :repex-per-kwp         232.36
         :repex-per-m2          11.08
         :repex-interval        20

         :size-for             :space-heat
         :efficiency           2.593}

        fuel
        {"Domestic Electricity"
         [[0.0 0.12511]
          [0.0 0.12511]
          [0.0 0.12511]]}
        ]
    (binding [finance/*npv-term* 2
              finance/*npv-rate* 0]
      (evaluate-alternative
       dom-ashp
       1
       1
       1
       1
       1
       fuel
       )
      ))

  )

(t/deftest test-evaluate-alternative
  (finance/with-parameters {:finance/npv-term 10
                            :finance/npv-rate 0}
    (let [cost-types ["capex" "opex" "repex"]
          units ["fixed" "per-kwh" "per-kwp" "per-m2" "per-connection"]
          w     {"fixed" 1 "per-kwh" 1 "per-kwp" 1 "per-m2" 1000 "per-connection" 10000}
          ]
      (doseq [cost-type cost-types
              unit units]
        (let [field (keyword (str cost-type "-" unit))
              result (#'sut/evaluate-alternative
                      {field 1
                       :repex-interval 2
                       :efficiency 1.0
                       :size-for :space-and-water}
                      10 0 100 1000 10000 {})

              cost (:cost result)
              cost%kwp (:cost%kwp result)
              cost%kwh (:cost%kwh result)
              ]
          (t/is (=
                 (* (double (w unit))
                    (case cost-type
                      "capex" 1.0
                      "opex"  9.0
                      "repex" 4.0))
                 
                 (case unit
                   ("fixed" "per-m2" "per-connection") cost
                   "per-kwp" cost%kwp
                   "per-kwh" cost%kwh))
                (str cost-type "-" unit))
          
          ))))
  
  )


;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.solver.interop-test
  (:require [thermos-backend.solver.interop :as sut]
            [clojure.test :as t]

            [thermos-specs.document :as document]
            [thermos-specs.supply :as supply]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            ))

(defn- dp4 [n] (/ (Math/round (* n 1000.0)) 1000.0))

(t/deftest tank-factor-npv-terms
  (let [;; this is cheating to get hold of a private function
        ;; #' gets the Var containing the function, and
        ;; Var implements IFn by delegating to what is inside it.
        alternative-definitions #'sut/alternative-definitions

        document
        #::document
        {:alternatives
         {0 #::supply
          {
           :cost-per-kwh      0.1
           :opex-per-kwp      3.0
           :capex-per-kwp     5.0
           :capex-per-mean-kw 2.0
           :fixed-cost        100.0
           :kwp-per-mean-kw   6.0
           
           }}
         :consider-alternatives true
         :npv-rate 0
         :npv-term 2
         }
        candidate #::demand {:alternatives #{0}}



        {:keys [cost cost%kwh cost%kwp]}
        (-> (alternative-definitions document candidate)
            (:alternatives) (first))

        document (update-in document [::document/alternatives 0]
                            dissoc ::supply/kwp-per-mean-kw)

        {cost-notank :cost cost%kwh-notank :cost%kwh cost%kwp-notank :cost%kwp}
        (-> (alternative-definitions document candidate)
            (:alternatives) (first))
        ]
    

    (t/is (== cost 100.0))
    (t/is (== cost%kwp 0)) ;; because we have shifted all costs onto kWh

    ;; only 4 decimals because there's a bit of wibbling around in NPV
    ;; calculation & float opeation order.
    (t/is (== (dp4 cost%kwh)
              (dp4 (+ 0.2 ;; fuel cost for 2 years
                      (/ (+ 2.0 ;; this is per mean kW
                            (* 6.0
                               (+ 5.0 ;; capex
                                  6.0 ;; 2y opex
                                  )
                               )) 8760.0)))))


    (t/is (== cost-notank 100.0)) ;; unchanged

    (t/is (== cost%kwp-notank (+ 5.0 6.0)))

    (t/is (== (dp4 cost%kwh-notank) (dp4 (+ 0.2 (/ 2.0 8760.0)))))
    ))


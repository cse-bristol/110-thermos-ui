;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.solver.interop-test
  (:require [thermos-backend.solver.interop :as sut]
            [clojure.test :as t]

            [thermos-specs.document :as document]
            [thermos-specs.supply :as supply]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.path :as path]
            [thermos-specs.defaults :as defaults]
            [thermos.opt.net.specs :as specs]
            [thermos.opt.net.pipes :as pipes]
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


(defn- street-document
  "A street of `n` buildings, each joined to the next by a 30m path,
  with a supply at the end."
  [n]
  (let [junction #(str "j" %)
        building (fn [i]
                   #::candidate{:id (str "b" i) :type :building :inclusion :optional
                                :selected false :connections [(junction i)]
                                ::demand/kwh 20000.0 ::demand/kwp 15.0
                                ::demand/connection-count 1})
        street   (fn [i]
                   #::candidate{:id (str "p" i) :type :path :inclusion :optional :selected false
                                ::path/start (junction i) ::path/end (junction (inc i))
                                ::path/length 30.0 ::path/civil-cost-id (inc (mod i 2))})
        supply   (assoc (building n)
                        ::supply/capacity-kwp 5000.0 ::supply/cost-per-kwh 0.02
                        ::supply/capex-per-kwp 100.0 ::supply/fixed-cost 1000.0)]
    (assoc defaults/default-document
           ::document/candidates
           (into {} (map (juxt ::candidate/id identity))
                 (concat (map building (range n)) [supply] (map street (range n)))))))

(defn- model-input [doc]
  (#'sut/instance->model-input doc
                               (sut/simplified-graph doc)
                               (sut/doc->pipe-curves doc)
                               (#'sut/make-market-decisions doc)))

(t/deftest model-input-is-valid
  (let [doc (-> (street-document 5)
                (assoc-in [::document/emissions-limit :enabled :co2] true)
                (assoc-in [::document/emissions-limit :value :co2] 1000.0))
        input (model-input doc)]
    (t/is (specs/ensure-valid-problem input))
    (t/is (not (contains? input :supply-limit)))
    (t/is (vector? (:kwp (:pipe-losses input))))
    (t/is (= 1000.0 (get-in input [:emissions :co2 :maximum])))
    (t/is (= 5 (count (filter :cost-steps (:edges input))))
          "every street has cost steps, and connectors (zero length) don't")
    (t/is (= 3 (:supply-limit
                (model-input (assoc doc ::document/maximum-supply-sites 3)))))))

(t/deftest pipe-losses-are-watts
  (let [doc (street-document 1)
        curves (sut/doc->pipe-curves
                (assoc-in doc [::document/pipe-costs :rows 100 :losses-kwh] 87.66))
        row (first (filter #(== 0.1 (:diameter %)) (pipes/curve-rows curves)))]
    ;; 87.66 kWh/m.yr is 10 W/m
    (t/is (< (Math/abs (- 10.0 (:losses-w row))) 1e-6))))

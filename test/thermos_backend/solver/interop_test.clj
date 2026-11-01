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

(defn- existing [doc path-id dia]
  (update-in doc [::document/candidates path-id]
             #(cond-> (assoc % ::path/exists true)
                dia (assoc ::path/maximum-diameter dia))))

(defn- model-edge [input a b]
  (first (filter #(= #{a b} #{(:i %) (:j %)}) (:edges input))))

(def ^:private recurring-pipework
  ;; so that replacing an existing pipe costs something
  {:pipework {:recur true :period 20}})

(t/deftest existing-pipes-can-be-upgraded
  (let [doc (-> (street-document 5)
                (assoc ::document/capital-costs recurring-pipework)
                (existing "p1" 0.02)
                (assoc-in [::document/candidates "p2" ::path/maximum-diameter] 0.02))
        curves (sut/doc->pipe-curves doc)
        input  (model-input doc)
        kept   (model-edge input "j1" "j2")
        capped (model-edge input "j2" "j3")
        new    (model-edge input "j3" "j4")]
    (t/is (specs/ensure-valid-problem input))

    (t/is (== (float (pipes/dia->kw curves 20.0)) (:existing-capacity%kwp kept)))
    (t/is (== (float (pipes/max-kw curves)) (:max-capacity%kwp kept))
          "the existing diameter is no limit")
    (t/is (pos? (:cost%m kept)))
    (t/is (zero? (:cost%kwm kept)))
    (let [[[kw cost] & more] (:cost-steps kept)]
      (t/is (== (pipes/max-kw curves) kw))
      (t/is (< (Math/abs (- cost (:cost%m kept))) 1e-3))
      (t/is (nil? more)))
    (t/is (not (neg? (:upgrade-cost%m kept))))
    (t/is (pos? (:upgrade-cost%kwm kept)))

    (t/is (== (float (pipes/dia->kw curves 20.0)) (:max-capacity%kwp capped))
          "a new path's maximum diameter is still a limit")
    (t/is (not (contains? capped :existing-capacity%kwp)))
    (t/is (not (contains? new :existing-capacity%kwp)))))

(t/deftest existing-pipes-of-unknown-size
  (let [doc   (-> (street-document 3)
                  (assoc ::document/capital-costs recurring-pipework))
        new   (model-edge (model-input doc) "j1" "j2")
        old   (model-edge (model-input (existing doc "p1" nil)) "j1" "j2")]
    (t/is (not (contains? old :existing-capacity%kwp)))
    (t/is (< (:cost%m old) (:cost%m new))
          "an existing pipe only costs its replacement")))

(defn- split-street
  "A building joined to a supply by two 30m paths which meet at a junction"
  []
  (let [doc (street-document 1)]
    (-> doc
        (update ::document/candidates dissoc "p0")
        (update ::document/candidates assoc
                "pa" #::candidate{:id "pa" :type :path :inclusion :optional :selected false
                                  ::path/start "j0" ::path/end "jm"
                                  ::path/length 30.0 ::path/civil-cost-id 1}
                "pb" #::candidate{:id "pb" :type :path :inclusion :optional :selected false
                                  ::path/start "jm" ::path/end "j1"
                                  ::path/length 30.0 ::path/civil-cost-id 1}))))

(t/deftest existing-and-new-paths-are-not-joined
  (let [edges #(count (filter (comp pos? :length) (:edges (model-input %))))]
    (t/is (= 1 (edges (split-street))))
    (t/is (= 1 (edges (-> (split-street) (existing "pa" 0.05) (existing "pb" 0.05)))))
    (t/is (= 2 (edges (-> (split-street) (existing "pa" 0.05)))))
    (t/is (= 2 (edges (-> (split-street) (existing "pa" 0.05) (existing "pb" 0.1)))))))

(t/deftest upgraded-pipes-in-the-solution
  (let [doc    (-> (street-document 1)
                   (assoc ::document/capital-costs recurring-pipework)
                   (existing "p0" 0.05))
        graph  (sut/simplified-graph doc)
        curves (sut/doc->pipe-curves doc)
        exist-kw (pipes/dia->kw curves 50.0)
        solve  (fn [kw]
                 (-> (#'sut/merge-solution
                      doc graph curves {}
                      {:state :valid
                       :edges [{:i "j0" :j "j1" :capacity-kw kw :losses-kw 0.0 :diversity 1.0}]})
                     (get-in [::document/candidates "p0"])))
        kept     (solve (* 0.5 exist-kw))
        upgraded (solve (* 2.0 exist-kw))]
    (t/is (not (:thermos-specs.solution/upgraded kept)))
    (t/is (== 50.0 (:thermos-specs.solution/diameter-mm kept)))
    (t/is (== exist-kw (:thermos-specs.solution/max-capacity-kw kept)))
    (t/is (= :existing-pipe-capex (:type (:thermos-specs.solution/pipe-capex kept))))

    (t/is (:thermos-specs.solution/upgraded upgraded))
    (t/is (< 50.0 (:thermos-specs.solution/diameter-mm upgraded)))
    (t/is (= :pipe-capex (:type (:thermos-specs.solution/pipe-capex upgraded))))))

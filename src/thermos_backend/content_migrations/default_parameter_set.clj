;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.content-migrations.default-parameter-set
  "The system parameter set new networks start with, and the migration
  which installs it.

  The numbers come from the thermos-research cost review (PROPOSAL.md,
  model/cost-structure.edn and data/*.csv there), central case,
  pre-feasibility stage, financial (operator) perspective, in real
  2025 £ excluding VAT. That review describes a richer cost structure
  than a network document holds, so it is reduced here:

  - Capital costs include the on-costs for their basis and the
    pre-feasibility contingency, since documents have nowhere to show
    them separately. Pipes also get the route allowance on trench length.
  - Replacement is approximated with the capital cost recurrence
    period for each kind of cost (see `capital-costs`).
  - Development lump sums are in the energy centres' fixed cost, so
    they count once per supply site.
  - Connection costs and tariffs are per building, so per-dwelling
    items are turned into £/kW, taking a flat's peak as `kw-per-flat`.
  - There is nowhere for O&M or insurance as a share of capex, SPV
    management, or residual value, so they are left out.
  - Carbon isn't priced (it's the financial view); emissions factors
    are the 2026 GHG reporting factors, for limits and reporting.

  Data the review marks as private (HNZP, GHNF_EC) is not used: so no
  ground-source energy centre, and mine water uses the WSHP variable
  O&M."
  (:require [thermos-backend.db :as db]
            [thermos-backend.db.parameter-sets :as parameter-sets]
            [thermos-specs.defaults :as defaults]
            [thermos-specs.document :as document]
            [thermos-specs.parameters :as parameters]
            [thermos-specs.supply :as supply]
            [thermos-specs.tariff :as tariff]
            [honeysql.helpers :as h]
            [clojure.tools.logging :as log]))

(def set-name "THERMOS defaults (2025 prices)")

(def description
  (str "Central costs from CSE's 2026 cost review, in real 2025 £ excluding VAT, "
       "for pre-feasibility work from the operator's point of view: "
       "6% real discount rate over 40 years, retail energy prices, carbon not priced. "
       "Capital costs include fees and a 30% contingency, "
       "and pipe costs a 10% allowance on route length. "
       "Energy centres include £1.1m of development costs."))

;; Money helpers

(defn- round
  ([x] (Math/round (double x)))
  ([x places] (let [k (Math/pow 10 places)] (/ (Math/round (* k (double x))) k))))

;; Appraisal

(def npv-rate 0.06) ; real pre-tax, public sector hurdle rate (3.5-8%)
(def npv-term 40)

(def on-costs
  "Client on-costs on contractor's prices (:works basis): design 4%,
  legal and procurement 1%, CAR insurance 0.35%, client PM 2%, surveys
  1%. Energy centre composites are all-in, so get none."
  (+ 0.04 0.01 0.0035 0.02 0.01))

(def contingency 0.30) ; pre-feasibility, financial perspective
(def route-allowance 0.10)

(def works   (* (+ 1 on-costs) (+ 1 contingency)))
(def all-in  (+ 1 contingency))

(def capital-costs
  "Recurrence periods stand in for lifetimes. Supply: 50% of the heat
  pump is replaced at 20 years, which at 6% has the same present value
  as all of it at about 30 years. Connections are mostly substations and
  HIUs (20-25 years). Pipes last 50 years, longer than the appraisal.
  Gas boilers in the counterfactual last about 15 years."
  {:pipework    {:annualize false :recur false :period 50 :rate 0.04}
   :supply      {:annualize false :recur true  :period 30 :rate 0.04}
   :connection  {:annualize false :recur true  :period 25 :rate 0.04}
   :insulation  {:annualize false :recur false :period 30 :rate 0.04}
   :alternative {:annualize false :recur true  :period 15 :rate 0.04}})

;; Energy (2025 outturn) and emissions (GHG reporting factors 2026)

(def electricity-price 0.24)   ; energy centre, p/kWh 15-32
(def gas-price 0.047)          ; energy centre, 3-8
(def domestic-electricity-price 0.26)
(def domestic-gas-price 0.0649)
(def domestic-gas-standing 117)

(def electricity-co2 (+ 0.13096 0.01299)) ; generation + T&D losses, kg/kWh
(def gas-co2 0.18231)                     ; gross CV

;; Pipes: M(DN) + kv*S(DN) + p*W(DN), £/trench m, flow + return,
;; series 2 steel, contractor's prices

(defn- mechanical [dn] (+ 150 (* 3.0 dn)))
(defn- civils-volume [dn] (+ 250 (* 2.5 dn)))
(defn- reinstated-width [dn] (+ 0.85 (* 0.0025 dn)))

(def surfaces
  "[id name kv p], p in £/m2 of reinstatement"
  [[1 "Soft"        1.0 0]
   [2 "Footway"     1.0 270]
   [3 "Carriageway" 1.0 550]
   [4 "Urban dense" 1.4 730]])

(def pipe-diameters
  [20 25 32 40 50 65 80 100 125 150 200 250 300 400 450 500 600 700 800 900 1000])

(def pipe-costs
  (let [k (* works (+ 1 route-allowance))]
    {:rows
     (into {}
           (for [dn pipe-diameters]
             [(double dn)
              (into {:pipe (round (* k (mechanical dn)))}
                    (for [[id _ kv p] surfaces]
                      [id (round (* k (+ (* kv (civils-volume dn))
                                         (* p (reinstated-width dn)))))]))]))
     :civils (into {} (for [[id nom] surfaces] [id nom]))
     :default-civils 3}))

;; Connections

(def kw-per-flat
  "The peak each flat adds to a block, from THERMOS's peak regression
  (0.0005 kW per kWh/yr) for a flat using about 7 MWh/yr"
  3.5)

(def service-connection 1500) ; THERMOS routes the connector, so not the £4k with a 10 m branch
(def substation-fixed 5000)
(def substation-per-kw 40)
(def hiu 3000)          ; incl. meter, installed
(def risers 2500)       ; new communal risers and laterals, per flat
(def wet-system 5000)   ; for an electrically heated flat

(defn- connection-cost [id nom fixed per-kw]
  #::tariff{:cc-id id :name nom
            :fixed-connection-cost (round (* works fixed))
            :variable-connection-cost (round (* works per-kw))})

(def connection-costs
  (let [bulk (+ service-connection substation-fixed)
        per-flat #(+ substation-per-kw (/ % kw-per-flat))]
    {1 (connection-cost 1 "Building substation" bulk substation-per-kw)
     2 (connection-cost 2 "House with HIU" (+ service-connection hiu) 0)
     3 (connection-cost 3 "Flats, new HIUs" bulk (per-flat hiu))
     4 (connection-cost 4 "Flats, new HIUs and risers" bulk (per-flat (+ hiu risers)))
     5 (connection-cost 5 "Flats, electric to wet heating" bulk
                        (per-flat (+ hiu risers wet-system)))}))

;; Tariffs

(def tariffs
  {1 #::tariff{:id 1 :name "Domestic (house)"
               :unit-charge 0.11 :standing-charge 200 :capacity-charge 0}
   ;; the £200 standing charge per flat, as a capacity charge
   2 #::tariff{:id 2 :name "Domestic (flats)"
               :unit-charge 0.11 :standing-charge 0
               :capacity-charge (round (/ 200 kw-per-flat))}
   3 #::tariff{:id 3 :name "Non-domestic"
               :unit-charge 0.09 :standing-charge 0 :capacity-charge 60}})

;; Counterfactual / individual systems: building-level prices, so no
;; on-costs or contingency

(def alternatives
  {1 #::supply{:id 1 :name "Gas boiler (domestic)"
               :capex-per-connection 3000 :opex-per-connection (+ 120 domestic-gas-standing)
               :cost-per-kwh (round (/ domestic-gas-price 0.85) 4)
               :emissions {:co2 (round (/ gas-co2 0.85) 4)}}
   2 #::supply{:id 2 :name "Air source heat pump (domestic)"
               :capex-per-connection 13000 :opex-per-connection 150
               :cost-per-kwh (round (/ domestic-electricity-price 2.8) 4)
               :emissions {:co2 (round (/ electricity-co2 2.8) 4)}}
   3 #::supply{:id 3 :name "Gas boiler (commercial)"
               :capex-per-kwp 495 :opex-per-kwp 9.7
               :cost-per-kwh (round (/ gas-price 0.86) 4)
               :emissions {:co2 (round (/ gas-co2 0.86) 4)}}
   4 #::supply{:id 4 :name "Air source heat pump (commercial)"
               :capex-per-kwp 2090 :opex-per-kwp 10.2
               :cost-per-kwh (round (/ electricity-price 2.84) 4)
               :emissions {:co2 (round (/ electricity-co2 2.84) 4)}}})

;; Energy centres, per kW of the network's diversified peak. The sizing
;; rule is heat pumps at 60% of peak delivering 88% of the heat, gas
;; boilers at 110% for peak and backup, and a store of 0.075 m3/kW.

(def development
  "DPD £300k, commercialisation £750k, SPV set-up £50k; no contingency"
  (+ 300e3 750e3 50e3))

(def hp-share-of-peak 0.6)
(def hp-share-of-heat 0.88)
(def boiler-share-of-peak 1.1)
(def store-m3-per-kw 0.075)
(def kva-per-hp-kw 0.4)

(def gas-boilers {:fixed 55e3 :per-kw 50 :om-per-kw 2.5 :var-om 0.0012 :efficiency 0.88})
(def store {:fixed 80e3 :per-m3 420})
(def grid {:fixed 180e3 :per-kva 75})   ; 11 kV DNO connection
(def pumping-station 110)               ; £/kW, :works

(def heat-pumps
  "All-in heat pump energy centres. :max-kw is the top of the range the
  cost fit is for, in kW of heat pump. Mine water has no public
  variable O&M, so it has WSHP's."
  [{:name "Air source heat pump" :fixed 567e3 :per-kw 1703 :scop 2.6
    :om-per-kw 12 :var-om 0.002 :max-kw 10000}
   {:name "River or sea source heat pump" :fixed 565e3 :per-kw 2039 :scop 3.0
    :om-per-kw 15 :var-om 0.0025 :max-kw 20000}
   {:name "Mine water heat pump" :fixed 374e3 :per-kw 1693 :scop 3.0
    :om-per-kw 12 :var-om 0.0025 :max-kw 10000}
   {:name "Sewage heat pump" :fixed 274e3 :per-kw 1978 :scop 3.2
    :om-per-kw 12 :var-om 0.004 :max-kw 10000}
   {:name "Waste heat with heat pump" :fixed 224e3 :per-kw 1618 :scop 3.8
    :om-per-kw 12 :var-om 0.003 :max-kw 10000}])

(defn- heat-pump-centre [id {:keys [name fixed per-kw scop om-per-kw var-om max-kw]}]
  (let [fixed (+ development
                 (* all-in (+ fixed (:fixed gas-boilers) (:fixed store) (:fixed grid))))
        per-kw (+ (* all-in (+ (* hp-share-of-peak per-kw)
                               (* boiler-share-of-peak (:per-kw gas-boilers))
                               (* store-m3-per-kw (:per-m3 store))
                               (* hp-share-of-peak kva-per-hp-kw (:per-kva grid))))
                  (* works pumping-station))
        capacity (round (/ max-kw hp-share-of-peak) -3)
        boiler-heat (- 1 hp-share-of-heat)]
    #::supply{:id id
              :name (str name " energy centre")
              :capacity-kwp (double capacity)
              :capex-curve [[0 (round fixed)] [capacity (round (+ fixed (* per-kw capacity)))]]
              :opex-per-kwp (round (+ (* hp-share-of-peak om-per-kw)
                                      (* boiler-share-of-peak (:om-per-kw gas-boilers)))
                                   2)
              :cost-per-kwh (round (+ (* hp-share-of-heat (+ (/ electricity-price scop) var-om))
                                      (* boiler-heat (+ (/ gas-price (:efficiency gas-boilers))
                                                        (:var-om gas-boilers))))
                                   4)
              :emissions {:co2 (round (+ (* hp-share-of-heat (/ electricity-co2 scop))
                                         (* boiler-heat (/ gas-co2 (:efficiency gas-boilers))))
                                      4)}}))

(def gas-boiler-centre
  "Gas boilers at 110% of peak, in an energy centre building (£160k +
  £80/kW, with external works of 20% of that)"
  (let [building-fixed (* 1.2 160e3)
        building-per-kw (* 1.2 80)
        fixed (+ development
                 (* all-in (:fixed gas-boilers))
                 (* works building-fixed))
        per-kw (+ (* all-in boiler-share-of-peak (:per-kw gas-boilers))
                  (* works (+ building-per-kw pumping-station)))
        capacity 20000]
    #::supply{:name "Gas boiler energy centre"
              :capacity-kwp (double capacity)
              :capex-curve [[0 (round fixed)] [capacity (round (+ fixed (* per-kw capacity)))]]
              :opex-per-kwp (round (* boiler-share-of-peak (:om-per-kw gas-boilers)) 2)
              :cost-per-kwh (round (+ (/ gas-price (:efficiency gas-boilers)) (:var-om gas-boilers)) 4)
              :emissions {:co2 (round (/ gas-co2 (:efficiency gas-boilers)) 4)}}))

(def supply-technologies
  (let [techs (concat (map-indexed heat-pump-centre heat-pumps)
                      [gas-boiler-centre])]
    (into {} (map-indexed (fn [i t] [i (assoc t ::supply/id i)]) techs))))

;; Supply model: plant only, as it has no building or development costs

(def supply-model
  (let [flat (fn [x] (let [vals (vec (repeat 24 x))] (into {} (for [d (range 5)] [d vals]))))]
    {::supply/fuels
     {0 {:name "Electricity" :price (flat electricity-price) :co2 (flat electricity-co2)}
      1 {:name "Natural gas" :price (flat gas-price) :co2 (flat gas-co2)}}

     ::supply/plants
     {0 {:name "Gas boiler" :capacity-kwp 15000 :fuel 1
         :heat-efficiency (:efficiency gas-boilers)
         :chp false :power-efficiency nil :substation nil
         :capital-cost {:fixed (round (* all-in (:fixed gas-boilers)))
                        :per-kwp (round (* all-in (:per-kw gas-boilers)))
                        :per-kwh 0}
         :operating-cost {:fixed 0 :per-kwp (:om-per-kw gas-boilers)
                          :per-kwh (:var-om gas-boilers)}
         :lifetime 25}
      1 {:name "Air source heat pump" :capacity-kwp 15000 :fuel 0
         :heat-efficiency 2.6
         :chp false :power-efficiency nil :substation 0
         :capital-cost {:fixed (round (* all-in 567e3)) :per-kwp (round (* all-in 1703)) :per-kwh 0}
         :operating-cost {:fixed 0 :per-kwp 12 :per-kwh 0.002}
         :lifetime 20}}

     ::supply/storages
     ;; £420/m3 is about £10/kWh at 35K
     {0 {:name "Storage" :capacity-kwh 40000 :capacity-kwp 10000 :efficiency 0.95
         :capital-cost {:fixed (round (* all-in (:fixed store)))
                        :per-kwp 0
                        :per-kwh (round (* all-in (/ (:per-m3 store) (* 1.163 35))) 2)}
         :lifetime 40}}

     ::supply/objective
     (assoc (::supply/objective defaults/default-document)
            :accounting-period npv-term
            :discount-rate npv-rate)}))

;; The set

(def categories
  #{:objective :pumping :pipe-costs :tariffs :connection-costs
    :alternatives :supply-technologies :supply})

(def solver-keys
  "Objective keys which are about the solver, not the costs, which
  applying the set should leave alone"
  [::document/mip-gap ::document/param-gap ::document/maximum-runtime
   ::document/maximum-iterations ::document/solution-method ::document/solver
   ::document/no-loops ::document/objective-scale ::document/objective-precision
   ::document/edge-cost-precision ::document/maximum-supply-sites])

(def content
  (-> (parameters/extract defaults/default-document categories)
      (as-> c (apply dissoc c solver-keys))
      (merge
       {::document/objective :network
        ::document/consider-alternatives false
        ::document/consider-insulation false
        ::document/npv-rate npv-rate
        ::document/npv-term npv-term
        ::document/loan-rate 0.04   ; PWLB ~6.3% nominal, 3.7-4.5% real
        ::document/loan-term 25
        ::document/capital-costs capital-costs
        ;; the heat pump SCOPs are for about 70C flow, 35-40C return
        ::document/flow-temperature 70.0
        ::document/return-temperature 40.0
        ::document/ground-temperature 8.0

        ::document/pumping-overhead 0.015
        ::document/pumping-cost-per-kwh electricity-price
        ::document/pumping-emissions {:co2 electricity-co2}

        ::document/pipe-costs pipe-costs
        ::document/tariffs tariffs
        ::document/connection-costs connection-costs
        ::document/alternatives alternatives
        ::document/supply-technologies supply-technologies}
       supply-model)))

(defn migrate
  "Install the set as a system set, and make it the system default
  unless there already is one."
  [conn]
  (log/info "Installing the default parameter set")
  (let [id (parameter-sets/create-set!
            {:name set-name
             :description description
             :scope :system
             :content content}
            conn)]
    (when-not (-> (h/select :id)
                  (h/from :parameter-sets)
                  (h/where [:= :is-system-default true])
                  (db/fetch-one! conn))
      (-> (h/update :parameter-sets)
          (h/sset {:is-system-default true})
          (h/where [:= :id id])
          (db/execute! conn)))))

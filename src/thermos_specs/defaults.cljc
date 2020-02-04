(ns thermos-specs.defaults
  (:require [thermos-specs.document :as document]
            [thermos-specs.demand :as demand]
            [thermos-specs.supply :as supply]
            [thermos-specs.view :as view]
            [thermos-specs.path :as path]
            [thermos-specs.tariff :as tariff]))

(def default-emissions {})

(def default-tariffs
  {1 #::tariff {:id 1
                :name "Standard"
                :standing-charge 50
                :unit-charge 0.05
                :capacity-charge 0}})

(def default-civil-costs
  {2 #::path {:civil-cost-id 2
               :civil-cost-name "Soft"
               :fixed-cost 200
               :variable-cost 200}

   1 #::path {:civil-cost-id 1
               :civil-cost-name "Hard"
               :fixed-cost 500
               :variable-cost 750}})

(def default-insulation {})

(def default-alternatives
  {1 #::supply {:id 1 :name "Gas CH"
                :cost-per-kwh 0.05
                :fixed-cost 2000
                :cost-per-kwp 100
                :opex-per-kwp 0
                :emissions
                {:co2 0.215}}})

(def default-document
  {::view/view-state
   {::view/map-layers {::view/basemap-layer :satellite ::view/candidates-layer true}
    ::view/map-view ::view/constraints
    ::view/show-pipe-diameters false}

   ::document/mip-gap 0.1
   ::document/maximum-runtime 0.5

   ::document/loan-term 25
   ::document/loan-rate 0.04

   ::document/npv-term 40
   ::document/npv-rate 0.03

   ::document/maximum-pipe-diameter 2.0
   ::document/minimum-pipe-diameter 0.02

   ::document/civil-cost-exponent 1.1
   ::document/mechanical-cost-exponent 1.3

   ::document/mechanical-cost-per-m  50.0
   ::document/mechanical-cost-per-m2 700.0

   ::document/flow-temperature 90.0
   ::document/return-temperature 60.0
   ::document/ground-temperature 8.0

   ::document/consider-insulation false
   ::document/consider-alternatives false
   ::document/objective :network

   ::document/tariffs default-tariffs
   ::document/civil-costs default-civil-costs
   ::document/insulation default-insulation
   ::document/alternatives default-alternatives

   ::tariff/market-term 10
   ::tariff/market-discount-rate 0.035
   ::tariff/market-stickiness 0.1})

(def cooling-parameters
  {::document/flow-temperature 5.5
   ::document/return-temperature 13.0
   ::document/ground-temperature 10.0})

(def default-cooling-document
  (merge default-document cooling-parameters))



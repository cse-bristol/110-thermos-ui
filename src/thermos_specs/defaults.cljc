(ns thermos-specs.defaults
  (:require [thermos-specs.document :as document]
            [thermos-specs.demand :as demand]
            [thermos-specs.view :as view]))

(def default-emissions {})

(def default-document
  {::view/view-state
   {::view/map-layers {::view/basemap-layer :satellite ::view/candidates-layer true}}

   ::demand/price 0.05
   ::demand/emissions {:co2 (/ 0.216 0.8) :nox 0 :pm25 0}

   ::document/mip-gap 0.1
   ::document/maximum-runtime 3 ;; hours

   ::document/loan-term 25
   ::document/loan-rate 0.04

   ::document/npv-term 40
   ::document/npv-rate 0.03

   ::document/maximum-pipe-kwp 20000

   ::document/civil-cost-exponent 1.1
   ::document/mechanical-cost-exponent 1.3

   ::document/mechanical-cost-per-m  50.0
   ::document/mechanical-cost-per-m2 700.0


   ::document/flow-temperature 90.0
   ::document/return-temperature 60.0
   ::document/ground-temperature 8.0
   })


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
   
   ;; ::document/emissions-factor {:co2e {:gas 0.216 :electricity 0.5 :biomass 0.016
   ;;                                     :heat 0.25
   ;;                                     }
   ;;                              :pm25 {:gas 0.1 :electricity 0 :biomass 0.3
   ;;                                     :heat 0.02
   ;;                                     }
   ;;                              }

   })


(ns thermos-ui.specs.defaults
  (:require [thermos-ui.specs.document :as document]
            [thermos-ui.specs.technology :as technology]
            [thermos-ui.specs.view :as view]
            ))

(declare large-chp small-chp small-boiler large-boiler heat-pump)

;; Name           Fuel        MW   Heat% Power% Cost

(def technologies
  [
   #::technology {:id "Small CHP"        :fuel :gas         :capacity 1    :heat-efficiency 0.70 :power-efficiency 0.30 :capital-cost 100000}
   #::technology {:id "Large CHP"        :fuel :gas         :capacity 10   :heat-efficiency 0.60 :power-efficiency 0.40 :capital-cost 800000}
   #::technology {:id "Biomass Boiler"   :fuel :biomass     :capacity 1    :heat-efficiency 0.90                        :capital-cost 60000}
   #::technology {:id "Tiny Gas Boiler"  :fuel :gas         :capacity 0.05 :heat-efficiency 0.90                        :capital-cost 2500}
   #::technology {:id "Small Gas Boiler" :fuel :gas         :capacity 1    :heat-efficiency 0.95                        :capital-cost 30000}
   #::technology {:id "Large Gas Boiler" :fuel :gas         :capacity 10   :heat-efficiency 0.95                        :capital-cost 250000}
   #::technology {:id "Small Heat Pump"  :fuel :electricity :capacity 1    :heat-efficiency 2.50                        :capital-cost 800000}
   #::technology {:id "Large Heat Pump"  :fuel :electricity :capacity 10   :heat-efficiency 4.00                        :capital-cost 5000000}
   ])

(def default-document
  {::document/technologies technologies

   ::view/view-state
   {::view/map-layers {::view/basemap-layer :satellite
                       ::view/candidates-layer true}}
   
   ::document/objective
   {::document/plant-period 7
    ::document/plant-discount-rate 3.5
    ::document/network-period 7
    ::document/network-discount-rate 3.5
    
    ::document/biomass-price 2.5
    ::document/electricity-import-price 10.13
    ::document/electricity-export-price 4.5
    ::document/gas-price 2.15

    ::document/heat-price 2.0

    ::document/electricity-emissions 0.394
    ::document/gas-emissions 0.184
    ::document/biomass-emissions 0.00432

    ::document/carbon-cost 0
    ::document/carbon-cap 1e12
    ::document/gap 0.05
    }
   }
  )

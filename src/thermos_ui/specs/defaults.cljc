(ns thermos-ui.specs.defaults
  (:require [thermos-ui.specs.document :as document]
            [thermos-ui.specs.technology :as technology]
            [thermos-ui.specs.view :as view]
            ))

;; Name           Fuel        kW   Heat% Power% Cost

(def technologies
  [
   #::technology {:id "Small CHP"        :fuel :gas         :capacity 1000    :heat-efficiency 0.70 :power-efficiency 0.30 :capital-cost 100000}
   #::technology {:id "Large CHP"        :fuel :gas         :capacity 10000   :heat-efficiency 0.60 :power-efficiency 0.40 :capital-cost 800000}
   #::technology {:id "Biomass Boiler"   :fuel :biomass     :capacity 1000    :heat-efficiency 0.90                        :capital-cost 60000}
   #::technology {:id "Tiny Gas Boiler"  :fuel :gas         :capacity 50 :heat-efficiency 0.90                        :capital-cost 2500}
   #::technology {:id "Small Gas Boiler" :fuel :gas         :capacity 1000    :heat-efficiency 0.95                        :capital-cost 30000}
   #::technology {:id "Large Gas Boiler" :fuel :gas         :capacity 10000   :heat-efficiency 0.95                        :capital-cost 250000}
   #::technology {:id "Small Heat Pump"  :fuel :electricity :capacity 1000    :heat-efficiency 2.50                        :capital-cost 800000}
   #::technology {:id "Large Heat Pump"  :fuel :electricity :capacity 10000   :heat-efficiency 4.00                        :capital-cost 5000000}
   ])


(def default-emissions {})

(def default-document
  {::document/technologies technologies

   ::view/view-state
   {::view/map-layers {::view/basemap-layer :satellite
                       ::view/candidates-layer true}}
   
   ::document/in-price {:gas 0.0348 :electricity 0.1 :biomass 0.0307}
   ::document/out-price {:electricity 0.09 :heat 0.0424}
   
   ::document/emissions-factor {:co2e {:gas 0.216 :electricity 0.5 :biomass 0.016
                                       :heat 0.25
                                       }
                                :pm25 {:gas 0.1 :electricity 0 :biomass 0.3
                                       :heat 0.02
                                       }
                                }

   ::document/emissions-price {:co2e 0.03 :pm25 0.01}
   ::document/emissions-cap {:co2e 0 :pm25 0}
   
   ::document/mip-gap 0.05
   ::document/system-term 50
   ::document/system-rate 0.04
   ::document/plant-term 25
   ::document/plant-rate 0.04
   ::document/network-term 25
   ::document/network-rate 0.04})


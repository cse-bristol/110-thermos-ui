(ns thermos-ui.specs.defaults
  (:require [thermos-ui.specs.document :as document]
            [thermos-ui.specs.technology :as technology]
            [thermos-ui.specs.view :as view]
            ))

(declare large-chp small-chp small-boiler large-boiler heat-pump)

(def large-chp
  {::technology/id "Large CHP"
   ::technology/fuel :gas
   ::technology/capacity 5
   ::technology/capital-cost 10000000
   ::technology/heat-efficiency 0.2
   ::technology/power-efficiency 0.4
   }
  )

(def small-chp
  (assoc large-chp
         ::technology/capacity 1
         ::technology/capital-cost 1000000
         ::technology/id "Small CHP"
         ))

(def large-boiler
  {::technology/id "Gas Boiler"
   ::technology/fuel :gas
   ::technology/capacity 1
   ::technology/capital-cost 500000
   ::technology/heat-efficiency 0.3
   }
  )

(def small-boiler
  {::technology/id  "Biomass Boiler"
   ::technology/fuel :biomass
   ::technology/capacity 1
   ::technology/capital-cost 600000
   ::technology/heat-efficiency 0.35
   }
  )

(def heat-pump
  {::technology/id  "Heat Pump"
   ::technology/fuel :electricity
   ::technology/capacity 2
   ::technology/capital-cost 1200000
   ::technology/heat-efficiency 2.5
   }
  )

(def default-document
  {::document/technologies
   [small-chp large-chp small-boiler large-boiler heat-pump]

   ::view/view-state
   {::view/map-layers {::view/basemap-layer :satellite
                       ::view/candidates-layer true}}
   
   ::document/objective
   {::document/period 7
    ::document/discount-rate 3.5

    ::document/biomass-price 2.5
    ::document/electricity-import-price 10.13
    ::document/electricity-export-price 4.5
    ::document/gas-price 2.15

    ::document/heat-price 2.0

    ::document/electricity-emissions 0.394
    ::document/gas-emissions 0.184
    ::document/biomass-emissions 0.00432
    }
   }
  )

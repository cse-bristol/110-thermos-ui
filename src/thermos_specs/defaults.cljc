(ns thermos-specs.defaults
  (:require [thermos-specs.document :as document]
            [thermos-specs.demand :as demand]
            [thermos-specs.supply :as supply]
            [thermos-specs.view :as view]
            [thermos-specs.path :as path]
            [thermos-specs.tariff :as tariff])
  #?(:cljs (:require-macros [thermos-specs.defaults :refer [current-version]])))

#?(:clj
   (defmacro current-version []
     (count @(requiring-resolve 'thermos-backend.content-migrations.piecewise/migrations))))

(def default-emissions {})

(def default-tariffs
  {1 #::tariff {:id 1
                :name "Standard"
                :standing-charge 50
                :unit-charge 0.05
                :capacity-charge 0}})

(def default-pipe-costs
  {:rows
   {1000.0 {:pipe 5046, 2 1180, 1 5965},
    65 {:pipe 193, 2 228, 1 656},
    20 {:pipe 81, 2 206, 1 534},
    300 {:pipe 1094, 2 405, 1 1642},
    450 {:pipe 1819, 2 547, 1 2435},
    50 {:pipe 152, 2 220, 1 611},
    32 {:pipe 107, 2 211, 1 562},
    40 {:pipe 126, 2 215, 1 583},
    600 {:pipe 2622, 2 705, 1 3313},
    500 {:pipe 2079, 2 598, 1 2719},
    150 {:pipe 474, 2 283, 1 964},
    100 {:pipe 300, 2 249, 1 774},
    800 {:pipe 3788, 2 933, 1 4589},
    250 {:pipe 874, 2 362, 1 1401},
    25 {:pipe 91, 2 208, 1 545},
    125 {:pipe 385, 2 266, 1 866},
    200 {:pipe 667, 2 321, 1 1174},
    900 {:pipe 4407, 2 1055, 1 5265},
    700 {:pipe 3192, 2 817, 1 3937},
    400 {:pipe 1568, 2 498, 1 2161},
    80 {:pipe 237, 2 237, 1 705}},
   :civils {2 "Soft", 1 "Hard"}
   :default-civils 1})

(def default-insulation {})

(def default-alternatives
  {1 #::supply {:id 1 :name "Gas CH"
                :cost-per-kwh 0.05
                :fixed-cost 2000
                :cost-per-kwp 100
                :opex-per-kwp 0
                :emissions
                {:co2 0.215}}})

(def default-supply-model-params
  (let [flat (fn [x]
               (let [vals (vec (repeat 24 x))]
                 {0 vals 1 vals 2 vals 3 vals 4 vals}))]
    {
     :thermos-specs.supply/day-types
     {0 {:divisions 24
         :name      "Normal weekday"
         :frequency 197}
      
      1 {:divisions 24
         :name      "Normal weekend"
         :frequency 78}
      
      2 {:divisions 24
         :name      "Winter weekday"
         :frequency 62}
      
      3 {:divisions 24
         :name      "Winter weekend"
         :frequency 27}
      
      4 {:divisions 24
         :name      "Peak day"
         :frequency 1}
      }

     :thermos-specs.supply/heat-profiles
     {0 {:name "Residential"
         :demand
         {4 [14.52 14.59 13.23 11.06 9.73 9.01 6.40 3.47 2.42 1.92 1.69 1.65 1.62 1.60 1.77 2.42 3.05 3.14 2.89 2.59 2.27 1.81 2.06 8.50]
          3 [10.83 10.93 9.92 8.28 7.23 6.73 4.91 2.86 2.19 1.87 1.68 1.63 1.54 1.46 1.59 2.13 2.59 2.59 2.35 2.05 1.77 1.41 1.58 6.30]
          2 [10.98 11.11 10.09 8.48 7.59 7.29 5.43 3.01 1.95 1.48 1.30 1.28 1.27 1.26 1.44 2.08 2.66 2.73 2.48 2.20 1.92 1.52 1.62 6.40]
          0 [3.54 4.52 4.07 3.44 3.26 3.82 3.58 2.17 1.11 0.76 0.65 0.67 0.64 0.60 0.78 1.39 1.83 1.80 1.56 1.32 1.08 0.79 0.58 1.54]
          1 [3.39 4.34 3.97 3.31 2.95 3.25 3.09 2.07 1.34 1.06 0.95 0.96 0.85 0.76 0.88 1.41 1.76 1.71 1.49 1.22 1.00 0.73 0.57 1.50]}
         
         }
      1 {:name "Commercial"
         :demand
         {4 [1.11 0.96 0.89 0.88 1.59 2.52 12.01 16.31 18.44 19.03 18.61 17.84 17.12 16.47 15.09 13.62 12.13 8.79 5.42 3.65 2.68 2.52 1.64 1.21]
          3 [0.82 0.70 0.63 0.60 0.91 1.14 4.58 5.39 5.19 6.01 6.84 6.67 6.26 5.81 5.42 4.94 3.71 2.42 1.74 1.66 1.78 1.67 1.16 0.89]
          2 [0.88 0.77 0.72 0.72 1.37 2.30 11.34 15.74 18.21 18.57 17.79 17.00 16.38 15.88 14.49 13.07 11.92 8.75 5.24 3.34 2.21 2.08 1.33 0.96]
          0 [0.36 0.29 0.26 0.25 0.52 0.74 3.71 4.73 9.01 10.41 10.08 9.35 8.95 9.57 8.26 7.39 7.09 5.63 1.15 1.04 1.00 0.88 0.52 0.39]
          1 [0.35 0.27 0.24 0.22 0.35 0.49 1.96 2.54 2.72 3.36 3.72 3.49 3.28 3.30 2.97 2.71 2.21 1.73 0.81 0.74 0.81 0.70 0.46 0.37]
          }}
      
      2 {:name   "Flat"
         :demand (flat 1.0)}}

     :thermos-specs.supply/default-profile 0
     
     :thermos-specs.supply/fuels
     {0 {:name  "Electricity"
         :price (flat 0.05)}
      
      1 {:name  "Natural gas"
         :price (flat 0.03)}}

     :thermos-specs.supply/grid-offer
     (flat 0.075)

     :thermos-specs.supply/plants
     {0 {:name             "Gas boiler"
         :capacity-kwp     15000
         :heat-efficiency  0.95
         :capital-cost     {:fixed 2000 :per-kwp 20 :per-kwh 0}
         :operating-cost   {:fixed 0 :per-kwp 0 :per-kwh 0}
         :fuel             1
         :chp              false
         :power-efficiency nil
         :lifetime         50
         :substation       nil}
      
      1 {:name             "Gas CHP"
         :capacity-kwp     4000
         :heat-efficiency  0.3
         :power-efficiency 0.6
         :fuel             1
         :capital-cost     {:fixed 5000 :per-kwp 50 :per-kwh 0}
         :operating-cost   {:fixed 0 :per-kwp 0 :per-kwh 0.08} ;; not sure
         :chp              true
         :lifetime         50
         :substation       0
         }
      2 {:name             "Heat pump"
         :substation       0
         :capacity-kwp     (* 3 5000)
         :heat-efficiency  3.0
         :power-efficiency nil
         :chp              false
         :fuel             0
         :capital-cost     {:fixed 10000 :per-kwp 100 :per-kwh 0}
         :operating-cost   {:fixed 0 :per-kwp 0 :per-kwh 0}
         :lifetime         50
         }
      }

     :thermos-specs.supply/storages
     {0 {:name         "Storage"
         :capacity-kwh 40000
         :capacity-kwp 10000
         :efficiency   0.9
         ;; not sure I have fixed cost right
         :capital-cost {:fixed 1000 :per-kwp 10 :per-kwh 0}
         :lifetime     50
         }

      }

     :thermos-specs.supply/substations
     {0 {:name         "A substation"
         :headroom-kwp 40000
         :alpha        0.8
         }}

     :thermos-specs.supply/objective
     {:accounting-period 50
      :discount-rate     0.035
      :curtailment-cost  10000
      :can-dump-heat     false
      :mip-gap           0.03
      :time-limit        1
      }
     
     }
    )
  )

(def default-document
  (merge
   {::document/version (current-version)

    ::view/view-state
    {::view/map-layers {::view/basemap-layer :satellite ::view/candidates-layer true}
     ::view/map-view ::view/constraints
     ::view/show-pipe-diameters false}

    ::document/mip-gap 0.1
    ::document/maximum-runtime 0.5

    ::document/loan-term 25
    ::document/loan-rate 0.04

    ::document/npv-term 40
    ::document/npv-rate 0.03

    ::document/flow-temperature 90.0
    ::document/return-temperature 60.0
    ::document/ground-temperature 8.0

    ::document/consider-insulation false
    ::document/consider-alternatives false
    ::document/objective :network

    ::document/tariffs default-tariffs
    ::document/pipe-costs default-pipe-costs
    ::document/insulation default-insulation
    ::document/alternatives default-alternatives

    ::tariff/market-term 10
    ::tariff/market-discount-rate 0.035
    ::tariff/market-stickiness 0.1}

   default-supply-model-params
   ))

(def cooling-parameters
  {::document/flow-temperature 5.5
   ::document/return-temperature 13.0
   ::document/ground-temperature 10.0})

(def default-cooling-document
  (merge default-document cooling-parameters))



;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

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
                :name "Small - 30kW"
                :standing-charge 0
                :unit-charge 0.065
                :capacity-charge 106.0}

   2 #::tariff {:id 2
                :name "Med - 150kW"
                :standing-charge 0
                :unit-charge 0.065
                :capacity-charge 95.0}

   3 #::tariff {:id 3
                :name "Large - 300kW"
                :standing-charge 0
                :unit-charge 0.065
                :capacity-charge 91.0}

   4 #::tariff {:id 4
                :name "Larger"
                :standing-charge 0
                :unit-charge 0.065
                :capacity-charge 89.0}})

(comment
  (require '[clojure.data.csv :as csv])
  (require '[clojure.java.io :as io])
  (require '[com.rpl.specter :as S])
  (def profile-rows
    (let [input-data "data/gas-space.csv"]
      (with-open [f (io/reader input-data)]
        (let [[h & rows] (csv/read-csv f)]
          (doall (map (partial zipmap (map keyword h)) rows))
          ))))

  ;; read into {day type [hh hh ...]}
  (def shape-by-day
    (->> (group-by :day profile-rows)
         (S/multi-transform
          [S/MAP-VALS 
           (S/multi-path [S/ALL (S/terminal (juxt (comp read-string :halfhour) (comp read-string :shape)))]
                         [(S/terminal #(sort-by first %))]
                         [S/ALL (S/terminal second)]
                         [(S/terminal vec)]
                         )
           
           ])))
  (require '[clojure.string :as string])
  ;; resample down to hourly
  (def shape-resampled
    (->> shape-by-day
         (S/transform [S/MAP-VALS] #(partition-all 2 %))
         (S/transform [S/MAP-VALS S/ALL] (fn [[a b]] (* 0.5 (+ a b))))
         ;; also rename keys
         (S/transform [S/MAP-KEYS] (fn [k] (let [[m d] (string/split k #"-")]
                                             [(Integer/parseInt m) (keyword (string/lower-case d))]
                                             ))))
    
    )
  ;; resample down to seasons
  (require '[clojure.set :as set])
  (let [months (set (range 1 13))
        summer (set (range 5 11)) ;; june - sept + shoulders
        winter (set/difference months summer)
        ->3dp (fn [x] (/ (Math/round (* 100.0 x)) 100.0))
        crunch
        (fn [months days]
          (let [shapes (keep
                        (fn [ [[m d] s] ] (when (and (months m) (days d)) s))
                        shape-resampled)]
            ;; pointwise average
            (apply map
                   (fn [& args] (/ (reduce + 0 args) (double (count args))))
                   shapes)))
        weekday #{:wd}
        weekend #{:sa :su}
        
        peak-day
        (let [shapes (map second shape-resampled)]
          (loop [max-shape (first shapes)
                 shapes (rest shapes)
                 max-value (reduce max max-shape)]
            (if (empty shapes) max-shape
                (let [[s & shapes] shapes
                      mv (reduce max s)]
                  (recur
                   (if (> mv max-value) s max-shape)
                   shapes
                   (max mv max-value))))))
        ]
    (def resi-profile-demand
      {0 (vec (map ->3dp (crunch summer weekday))) 
       1 (vec (map ->3dp (crunch summer weekend)))
       2 (vec (map ->3dp (crunch winter weekday))) 
       3 (vec (map ->3dp (crunch winter weekend)))
       4 (vec (map ->3dp (map #(* 1.1 %) peak-day)))
       }
      )
    
    )
  ;; insert peak by multiplying up a bit

  
  )

(def default-pipe-costs
  {:civils {1 "soft"
            2 "city-centre"
            3 "motorway"
            4 "non-city-centre"
            5 "non-highway"
            6 "residential"}
   :rows
   {20.0     {:pipe 0.0, 1 501.0 , 2 1168.0, 3 1870.0,  4 993.0 , 5 552.0,  6 611.0,},
    25.0     {:pipe 0.0, 1 512.0 , 2 1195.0, 3 1912.0,  4 1015.0, 5 576.0,  6 636.0,},
    32.0     {:pipe 0.0, 1 523.0 , 2 1220.0, 3 1951.0,  4 1036.0, 5 588.0,  6 649.0,},
    40.0     {:pipe 0.0, 1 534.0 , 2 1244.0, 3 1991.0,  4 1057.0, 5 600.0,  6 662.0,},
    50.0     {:pipe 0.0, 1 549.0 , 2 1277.0, 3 2044.0,  4 1085.0, 5 616.0,  6 680.0,},
    65.0     {:pipe 0.0, 1 557.0 , 2 1298.0, 3 2077.0,  4 1103.0, 5 626.0,  6 692.0,},
    80.0     {:pipe 0.0, 1 603.0 , 2 1406.0, 3 2249.0,  4 1194.0, 5 678.0,  6 744.0,},
    100.0    {:pipe 0.0, 1 656.0 , 2 1530.0, 3 2448.0,  4 1300.0, 5 738.0,  6 820.0,},
    125.0    {:pipe 0.0, 1 765.0 , 2 1748.0, 3 2797.0,  4 1486.0, 5 885.0,  6 983.0,},
    150.0    {:pipe 0.0, 1 847.0 , 2 2049.0, 3 3278.0,  4 1742.0, 5 1106.0, 6 1229.0},
    200.0    {:pipe 0.0, 1 1038.0, 2 2459.0, 3 3934.0,  4 2090.0, 5 1328.0, 6 1475.0},
    250.0    {:pipe 0.0, 1 1202.0, 2 2732.0, 3 4371.0,  4 2322.0, 5 1475.0, 6 1639.0},
    300.0    {:pipe 0.0, 1 1311.0, 2 3278.0, 3 5245.0,  4 2786.0, 5 1623.0, 6 1803.0},
    350.0    {:pipe 0.0, 1 1475.0, 2 3606.0, 3 5770.0,  4 3065.0, 5 1869.0, 6 2076.0},
    400.0    {:pipe 0.0, 1 1639.0, 2 3825.0, 3 6119.0,  4 3251.0, 5 2065.0, 6 2295.0},
    450.0    {:pipe 0.0, 1 1803.0, 2 4152.0, 3 6644.0,  4 3530.0, 5 2262.0, 6 2513.0},
    500.0    {:pipe 0.0, 1 1912.0, 2 4589.0, 3 7343.0,  4 3901.0, 5 2508.0, 6 2786.0},
    600.0    {:pipe 0.0, 1 2076.0, 2 5157.0, 3 8252.0,  4 4383.0, 5 2828.0, 6 3141.0},
    700.0    {:pipe 0.0, 1 2239.0, 2 5725.0, 3 9160.0,  4 4866.0, 5 3148.0, 6 3496.0},
    800.0    {:pipe 0.0, 1 2402.0, 2 6293.0, 3 10069.0, 4 5348.0, 5 3467.0, 6 3851.0},
    900.0    {:pipe 0.0, 1 2566.0, 2 6861.0, 3 10978.0, 4 5830.0, 5 3787.0, 6 4206.0},
    1000.0   {:pipe 0.0, 1 2730.0, 2 7430.0, 3 11886.0, 4 6312.0, 5 4107.0, 6 4560.0},
    1100.0   {:pipe 0.0, 1 2894.0, 2 7999.0, 3 12794.0, 4 6794.0, 5 4427.0, 6 4914.0},
    1200.0   {:pipe 0.0, 1 3058.0, 2 8568.0, 3 13702.0, 4 7276.0, 5 4747.0, 6 5268.0}}
   :default-civils 6
   })

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
         :demand ;; generated by the comment above.
         {0 [0.02 0.02 0.02 0.03 0.09 0.24 0.36 0.28 0.18 0.11 0.09 0.09 0.09 0.09 0.12 0.21 0.32 0.36 0.36 0.33 0.26 0.16 0.08 0.04],
          1 [0.03 0.03 0.03 0.03 0.07 0.2 0.33 0.31 0.23 0.16 0.13 0.13 0.13 0.14 0.16 0.25 0.32 0.36 0.36 0.32 0.26 0.17 0.08 0.04],
          2 [0.32 0.27 0.26 0.27 0.34 0.68 1.67 2.33 1.97 1.45 1.2 1.11 1.17 1.22 1.29 1.58 2.15 2.48 2.43 2.29 2.05 1.67 1.08 0.54],
          3 [0.37 0.29 0.27 0.27 0.33 0.57 1.32 2.1 2.08 1.73 1.48 1.35 1.4 1.41 1.45 1.66 2.09 2.33 2.28 2.14 1.93 1.58 1.05 0.55],
          4 [0.33 0.25 0.23 0.24 0.29 0.56 1.5 2.41 2.18 1.56 1.15 0.95 0.99 0.96 0.96 1.14 1.77 2.24 2.41 2.31 2.01 1.54 0.92 0.42]}
         }
      1 {:name "Commercial"
         :demand
         {4 [1.11 0.96 0.89 0.88 1.59 2.52 12.01 16.31 18.44 19.03 18.61 17.84 17.12 16.47 15.09 13.62 12.13 8.79 5.42 3.65 2.68 2.52 1.64 1.21]
          3 [0.82 0.70 0.63 0.60 0.91 1.14 4.58 5.39 5.19 6.01 6.84 6.67 6.26 5.81 5.42 4.94 3.71 2.42 1.74 1.66 1.78 1.67 1.16 0.89]
          2 [0.88 0.77 0.72 0.72 1.37 2.30 11.34 15.74 18.21 18.57 17.79 17.00 16.38 15.88 14.49 13.07 11.92 8.75 5.24 3.34 2.21 2.08 1.33 0.96]
          0 [0.36 0.29 0.26 0.25 0.52 0.74 3.71 4.73 9.01 10.41 10.08 9.35 8.95 9.57 8.26 7.39 7.09 5.63 1.15 1.04 1.00 0.88 0.52 0.39]
          1 [0.35 0.27 0.24 0.22 0.35 0.49 1.96 2.54 2.72 3.36 3.72 3.49 3.28 3.30 2.97 2.71 2.21 1.73 0.81 0.74 0.81 0.70 0.46 0.37]
          }}
      
      2 {:name   "Uniform"
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
     ::view/show-pipe-diameters true}

    ::document/mip-gap 0.1
    ::document/maximum-runtime 0.5
    ::document/param-gap 0.01
    ::document/maximum-iterations 8

    ::document/loan-term 25
    ::document/loan-rate 0.04

    ::document/npv-term 40
    ::document/npv-rate 0.03

    ::document/flow-temperature 90.0
    ::document/return-temperature 60.0
    ::document/ground-temperature 8.0

    ::document/steam-pressure 1.6
    ::document/steam-velocity 20.0
    ::document/medium :hot-water
    
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



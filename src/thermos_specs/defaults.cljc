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
                :name "Standard"
                :standing-charge 50
                :unit-charge 0.05
                :capacity-charge 0}})

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
    ::tariff/market-stickiness 0.1

    ::document/diversity-limit 0.62
    ::document/diversity-rate 1.0

    ::document/objective-scale 1.0
    ::document/objective-precision 1.0
    ::document/edge-cost-precision 0.0
    }

   default-supply-model-params
   ))

(def cooling-parameters
  {::document/flow-temperature 5.5
   ::document/return-temperature 13.0
   ::document/ground-temperature 10.0})

(def default-cooling-document
  (merge default-document cooling-parameters))



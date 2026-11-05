;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.test-documents
  "Small network documents for tests of the things which export them
  (the spreadsheet and the GeoPackage)"
  (:require [thermos-specs.defaults :as defaults]
            [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.path :as path]
            [thermos-specs.solution :as solution]
            [thermos-specs.supply :as supply]
            [thermos-specs.tariff :as tariff]))

(defn- polygon [x y]
  (format "{\"type\":\"Polygon\",\"coordinates\":[[[%s,%s],[%s,%s],[%s,%s],[%s,%s],[%s,%s]]]}"
          x y (+ x 0.0001) y (+ x 0.0001) (+ y 0.0001) x (+ y 0.0001) x y))

(defn- point [x y]
  (format "{\"type\":\"Point\",\"coordinates\":[%s,%s]}" x y))

(defn- line [& xys]
  (format "{\"type\":\"LineString\",\"coordinates\":[%s]}"
          (clojure.string/join "," (for [[x y] (partition 2 xys)] (format "[%s,%s]" x y)))))

(defn- building [id geometry & {:as more}]
  (merge
   {::candidate/id id
    ::candidate/type :building
    ::candidate/geometry geometry
    ::candidate/selected false
    ::candidate/inclusion :optional
    ::candidate/connections ["p1"]
    ::candidate/wall-area 100.0
    ::candidate/ground-area 50.0
    ::candidate/roof-area 50.0
    ::candidate/user-fields {"Address" (str id " High Street") "Floors" 2}
    ::demand/kwh 10000.0
    ::demand/kwp 5.0
    ::demand/source "given"
    ::tariff/id 1
    ::tariff/cc-id 1}
   more))

(defn- path [id geometry length & {:as more}]
  (merge
   {::candidate/id id
    ::candidate/type :path
    ::candidate/geometry geometry
    ::candidate/selected false
    ::candidate/inclusion :optional
    ::candidate/user-fields {"Road" "High Street"}
    ::path/start "b1"
    ::path/end "b2"
    ::path/length length
    ::path/civil-cost-id 1}
   more))

(def unsolved-document
  "A network with three buildings, one of which is the supply and one
  of which is a point, and two paths, which hasn't been run"
  (-> defaults/default-document
      (assoc ::document/connection-costs
             {1 {::tariff/cc-id 1
                 ::tariff/name "Standard connection"
                 ::tariff/fixed-connection-cost 1000.0
                 ::tariff/variable-connection-cost 50.0}})
      (assoc ::document/candidates
             {"b1" (building "b1" (polygon -2.59 51.45)
                             ::supply/technology-id 0
                             ::supply/capacity-kwp 1000.0)
              "b2" (building "b2" (polygon -2.5895 51.4505))
              "b3" (building "b3" (point -2.5890 51.4510)
                             ::candidate/user-fields {"Address" "3 Low Road"})
              "p1" (path "p1" (line -2.59 51.45 -2.5895 51.4505) 70.5)
              "p2" (path "p2" (line -2.5895 51.4505 -2.5890 51.4510) 72.0
                         ::path/exists true)})))

(def solved-document
  "`unsolved-document`, with a solution in which b1 supplies b2 over p1,
  and b3 has an individual system"
  (let [cost (fn [x] {:principal x :present (* 0.9 x) :total (* 1.1 x)})
        opex (fn [x] {:annual x :present (* 9 x) :total (* 10 x)})]
    (-> unsolved-document
        (update ::document/candidates
                (fn [cs]
                  (-> cs
                      (update "b1" merge
                              {::solution/included true
                               ::solution/connected true
                               ::solution/capacity-kw 8.0
                               ::solution/diversity 0.9
                               ::solution/output-kwh 21000.0
                               ::solution/supply-capex (cost 50000.0)
                               ::solution/heat-cost (opex 600.0)
                               ::solution/pumping-kwh 20.0
                               ::solution/pumping-cost (opex 2.0)
                               ::solution/heat-revenue (opex 800.0)
                               ::solution/connection-capex (cost 1250.0)})
                      (update "b2" merge
                              {::solution/included true
                               ::solution/connected true
                               ::solution/heat-revenue (opex 800.0)
                               ::solution/connection-capex (cost 1250.0)})
                      (update "b3" merge
                              {::solution/alternative
                               {::supply/name "Gas boiler"
                                :capex (cost 2000.0)
                                :opex (opex 100.0)
                                :heat-cost (opex 400.0)
                                :emissions {:co2 {:kg 2000.0 :annual 10.0 :present 90.0 :total 100.0}}}})
                      (update "p1" merge
                              {::solution/included true
                               ::solution/diameter-mm 40.0
                               ::solution/capacity-kw 12.0
                               ::solution/losses-kwh 300.0
                               ::solution/diversity 0.9
                               ::solution/pipe-capex (cost 30000.0)}))))
        (assoc ::solution/state :valid
               ::solution/message "Optimal"
               ::solution/runtime 12
               ::solution/objective 12345.5
               ::solution/gap 0.01
               ::solution/bounds [12300.0 12400.0]
               ::solution/iterations 3
               ::solution/objectives [12000.0 12345.5 12345.5]
               ::solution/stages [:heuristic :mip]
               ::solution/stopped-early false
               ::solution/log "solver log"))))

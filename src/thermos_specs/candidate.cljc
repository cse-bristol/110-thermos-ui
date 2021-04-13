;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-specs.candidate
  (:require [clojure.spec.alpha :as s]
            [thermos-specs.solution :as solution]
            [thermos-specs.supply :as supply]
            [thermos-specs.demand :as demand]
            [thermos-specs.cooling :as cooling]
            [thermos-specs.tariff :as tariff]
            [thermos-specs.path :as path]))

(defn- nil-or-string? [x] (or (nil? x) (string? x)))

(s/def ::candidate
  (s/and
   (s/keys :req [::id ::type ::geometry ::selected ::inclusion]
           :opt [::solution/included
                 ::modified])
   (s/or :is-building ::building
         :is-path ::path/path)))

(s/def ::building
  (s/and
   #(= (::type % :building))
   (s/keys :req [::connections
                 ::wall-area
                 ;; ::floor-area

                 ::ground-area
                 ::roof-area
                 ;; ::height
                 ]
           :opt [::tariff/id])
   
   (s/or :has-demand ::demand/demand ;; TODO this is not quite right
         :has-supply ::supply/supply
         :has-nothing (constantly true))))

(s/def ::type #{:building :path})
(s/def ::selected boolean?)
(s/def ::id string?)
(s/def ::inclusion #{:required :optional :forbidden})
(s/def ::connections (s/* ::id))
(s/def ::modified boolean?) ;; a modified candidate is one the user has changed

(s/def ::wall-area number?)
(s/def ::roof-area number?)

;; this is distinct from floor area; it's relevant for insulation
;; practically it will likely be the same as roof area since we don't do pitch.
(s/def ::ground-area number?)

(defn is-included? [candidate] (not= :forbidden (::inclusion candidate :forbidden)))
(defn is-path? [candidate] (= (::type candidate) :path))
(defn is-building? [candidate] (= (::type candidate) :building))

(defn annual-demand
  [candidate mode]
  
  (when (is-building? candidate)
    (case mode
      :cooling (::cooling/kwh candidate 0)
      (::demand/kwh candidate 0))))

(defn peak-demand
  [candidate mode]
  
  (when (is-building? candidate)
    (case mode
      :cooling (::cooling/kwp candidate 0)
      (::demand/kwp candidate 0))))

(defn solved-peak-demand
  "A building may have solution data on it indicating a different peak (due to alternatives' tank-factor).
  This returns that if it's there, or `peak-demand`"
  [candidate mode]
  (or (::solution/kwp candidate) (peak-demand candidate mode)))

(defn solved-annual-demand
  "A building may have solution data on it indicating a different kwh (due to insulation).
  This returns that if it's there, or `annual-demand`"
  [candidate mode]
  (or (::solution/kwh candidate) (annual-demand candidate mode)))

(defn has-demand? [candidate mode]
  (when-let [^double demand (annual-demand candidate mode)]
    (pos? demand)))

(defn has-supply?
  "True if a building could be a supply location. To see whether it was used, see `supply-in-solution?`"
  [candidate]
  (when-let [^double cap (::supply/capacity-kwp candidate)]
    (pos? cap)))

(defn required? [candidate]
  (= (::inclusion candidate) :required))

(defn in-solution? [candidate] (::solution/included candidate))
(defn is-connected? [candidate]
  (or (and (is-path? candidate)
           (in-solution? candidate))
      (::solution/connected candidate)))

(defn supply-in-solution? [candidate]
  (and (is-building? candidate)
       (has-supply? candidate)
       (::solution/included candidate)
       (::solution/capacity-kw candidate)))

(def emissions-types [:co2 :pm25 :nox])

(def emissions-labels
  {:co2 [:span "CO" [:sub "2"]]
   :pm25 [:span "PM" [:sub "2.5"]]
   :nox [:span "NO" [:sub "x"]]})

(def text-emissions-labels
  {:co2 "CO₂"
   :pm25 "PM₂₅"
   :nox "NOₓ"})

(def emissions-factor-scales
  {:co2  1000.0    ;; grams
   :pm25 1000000.0 ;; milligrams
   :nox  1000000.0 ;; milligrams
   })

(def emissions-factor-units
  {:co2  "g/kWh"
   :pm25 "mg/kWh"
   :nox  "mg/kWh"})

(defn unreachable? [candidate] (::solution/unreachable candidate))
(defn peripheral? [candidate] (= :peripheral (::solution/unreachable candidate)))

(defn forbid-supply! [candidate]
  (dissoc candidate ::supply/capacity-kwp))

(defn got-alternative? [candidate]
  (and (::solution/alternative candidate)
       (not (:counterfactual (::solution/alternative candidate)))))

(defn got-counterfactual? [candidate]
  (and (::solution/alternative candidate)
       (:counterfactual (::solution/alternative candidate))))


(defn solution-description
  "A textual description of what has happened to a candidate, if there
  is a solution"
  [candidate]
  (cond
    (is-connected? candidate) "network"
    (got-alternative? candidate)
    (-> candidate
        (::solution/alternative)
        (::supply/name))

    (got-counterfactual? candidate)
    (-> candidate
        (::solution/alternative)
        (::supply/name)
        (str " (existing)"))

    (peripheral? candidate) "peripheral"
    (unreachable? candidate) "impossible"))

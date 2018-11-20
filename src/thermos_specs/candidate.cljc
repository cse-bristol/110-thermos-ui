(ns thermos-specs.candidate
  (:require [clojure.spec.alpha :as s]
            [thermos-specs.solution :as solution]
            [thermos-specs.supply :as supply]
            [thermos-specs.demand :as demand]
            [thermos-specs.path :as path]))

(defn- nil-or-string? [x] (or (nil? x) (string? x)))

(s/def ::candidate
  (s/and
   (s/keys :req [::id ::type ::subtype ::name ::geometry ::selected ::inclusion]
           :opt [::solution/included])
   (s/or :is-building ::building
         :is-path ::path/path)))

(s/def ::building
  (s/and
   #(= (::type % :building))
   (s/keys :req [::connections ])
   (s/or :has-demand ::demand/demand ;; TODO this is not quite right
         :has-supply ::supply/supply
         :has-nothing (constantly true))))

(s/def ::type #{:building :path})
(s/def ::selected boolean?)
(s/def ::id string?)
(s/def ::subtype nil-or-string?)
(s/def ::name nil-or-string?)
(s/def ::inclusion #{:required :optional :forbidden})
(s/def ::connections (s/* ::id))

(defn is-included? [candidate] (not= :forbidden (::inclusion candidate)))
(defn is-path? [candidate] (= (::type candidate) :path))
(defn is-building? [candidate] (= (::type candidate) :building))
(defn has-demand? [candidate]
  (when-let [demand (::demand/kwh candidate)] (pos? demand)))

(defn has-supply? [candidate]
  (when-let [cap (::supply/capacity-kwp candidate)]
    (pos? cap)))

(defn required? [candidate]
  (= (::inclusion candidate) :required))

(defn in-solution? [candidate] (::solution/included candidate))

(def emissions-types #{:co2 :pm25 :nox})

(defn unreachable? [candidate] (::solution/unreachable candidate))

(defn emissions [candidate e document]
  (* (::demand/kwh candidate 0)
     (or (get (::demand/emissions candidate) e)
         (get (::demand/emissions document) e 0))))

(defn forbid-supply! [candidate]
  (dissoc candidate ::supply/capacity-kwp))

(defn reset-defaults! [candidate]
  (dissoc candidate
          ::demand/emissions
          ::demand/price))

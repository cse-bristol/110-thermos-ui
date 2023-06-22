(ns thermos-specs.magic-fields
  "Some user-defined fields are used to decide on what civil costs etc.
  to use, in the absence of something the user chose.

  This is implemented by a single function in here, which modifies the
  whole state accordingly"

  (:require [com.rpl.specter :as S]
            [thermos-specs.document :as document]
            [thermos-specs.path :as path]
            [thermos-specs.candidate :as candidate]
            [thermos-util :as util]
            [clojure.set :as set]
            [thermos-specs.tariff :as tariff]
            [thermos-specs.demand :as demand]
            [thermos-specs.measure :as measure]
            [thermos-specs.supply :as supply]
            [clojure.string :as string]))

(def ^:const CIVIL-COST-FIELD "cost_category")
(def ^:const CONNECTION-COST-FIELD "CONNECTION-COST")
(def ^:const TARIFF-FIELD "TARIFF")
(def ^:const INSULATION-FIELD "INSULATION")
(def ^:const ALTERNATIVES-FIELD "INDIVIDUAL-SYSTEMS")
(def ^:const COUNTERFACTUAL-FIELD "COUNTERFACTUAL-SYSTEM")

(def ^:const SUPPLY-CAPACITY-FIELD    "capacity_kwp")
(def ^:const SUPPLY-CAPACITY-KWH-FIELD    "capacity_kwh")
(def ^:const SUPPLY-FIXED-CAPEX-FIELD "capex_fixed")
(def ^:const SUPPLY-FIXED-OPEX-FIELD  "opex_fixed")
(def ^:const SUPPLY-KWP-CAPEX-FIELD   "capex_per_kwp")
(def ^:const SUPPLY-KWP-COST-FIELD    "opex_per_kwp")
(def ^:const SUPPLY-KWH-COST-FIELD    "opex_per_kwh")
(def ^:const SUPPLY-NOX-FIELD         "SUPPLY-NOX-MG-PER-KWH")
(def ^:const SUPPLY-CO2-FIELD         "SUPPLY-CO2-G-PER-KWH")
(def ^:const SUPPLY-PM25-FIELD        "SUPPLY-PM25-MG-PER-KWH")

(def join-fields
  #{
     CIVIL-COST-FIELD
     CONNECTION-COST-FIELD
     TARIFF-FIELD
     INSULATION-FIELD
     ALTERNATIVES-FIELD
     COUNTERFACTUAL-FIELD
    }
  )


(defn join
  "Update document to use the magic fields for setting tariff,
  connection cost, insulation, and alternatives"
  [document & {:keys [user-fields-in] :or {user-fields-in ::candidate/user-fields}}]
  (let [civil-cost-by-name (-> document
                               (::document/pipe-costs)
                               (:civils)
                               (set/map-invert))
        tariff-by-name     (-> document
                               ::document/tariffs
                               (->> (reduce-kv
                                     (fn [a k v] (assoc a (::tariff/name v) k))
                                     {"Market" :market})))

        con-cost-by-name   (-> document
                               ::document/connection-costs
                               (->> (reduce-kv
                                     (fn [a k v] (assoc a (::tariff/name v) k))
                                     {})))

        insulation-by-name (-> document
                               ::document/insulation
                               (->> (reduce-kv
                                     (fn [a k v] (assoc a (::measure/name v) k))
                                     {})))

        alternative-by-name (-> document
                                ::document/alternatives
                                (->> (reduce-kv
                                      (fn [a k v] (assoc a (::supply/name v) k))
                                      {})))

        assoc-fields
        (fn [x]
          (let [user-fields (user-fields-in x)
                
                {civil-cost-name CIVIL-COST-FIELD
                 tariff-name TARIFF-FIELD
                 con-cost-name CONNECTION-COST-FIELD
                 insulation-names INSULATION-FIELD
                 alternative-names ALTERNATIVES-FIELD
                 counterfactual-name COUNTERFACTUAL-FIELD} user-fields
                
                civil-cost      (and civil-cost-name (civil-cost-by-name civil-cost-name))
                tariff          (and tariff-name     (tariff-by-name tariff-name))
                con-cost        (and con-cost-name   (con-cost-by-name con-cost-name))
                counterfactual  (and counterfactual-name (alternative-by-name counterfactual-name))

                insulations (when (and
                                      (string? insulation-names)
                                      (not (string/blank? insulation-names)))
                                (set (keep insulation-by-name (string/split insulation-names #"[,;] *"))))
                alternatives   (when (and
                                      (string? alternative-names)
                                      (not (string/blank? alternative-names)))
                                (set (keep alternative-by-name (string/split alternative-names #"[,;] *"))))
                ]
            (cond-> x
              (and civil-cost (not (::path/civil-cost-id x)))
              (assoc ::path/civil-cost-id civil-cost)

              (and tariff     (not (::tariff/id x)))
              (assoc ::tariff/id tariff)

              (and con-cost   (not (::tariff/cc-id x)))
              (assoc ::tariff/cc-id con-cost)

              (and counterfactual (not (::demand/counterfactual x)))
              (assoc ::demand/counterfactual counterfactual)

              (and  (seq insulations)   (not (::demand/insulation x)))
              (assoc ::demand/insulation insulations)

              (and (seq alternatives)    (not (::demand/alternatives x)))
              (assoc ::demand/alternatives alternatives))
            ))]

    ;; TODO profiling here - may be we need to speed this up?
    (S/transform
     [::document/candidates S/MAP-VALS
      (S/selected? user-fields-in S/MAP-KEYS join-fields)
      (S/selected?
       (S/multi-path
        ::path/civil-cost-id
        ::tariff/id
        ::tariff/cc-id
        ::demand/alternatives
        ::demand/insulation
        ::demand/counterfactual)
       nil?)
      ]
     assoc-fields
     document)))

(defn initialize
  "Initialize the configuration for candidate, when it has been loaded from database
  This will apply defaults for supply.
  "
  [candidate]

  (let [user-fields     (::candidate/user-fields candidate)
        supply-capacity-kw (get user-fields SUPPLY-CAPACITY-FIELD 0)
        ]
    
    (if (and (number? supply-capacity-kw) (zero? supply-capacity-kw))
      candidate
      (let [field (fn [k]
                    (-> user-fields
                        (get k 0.0)
                        (util/as-double)
                        (or 0.0)))
            nil-field (fn [k]
                        (-> user-fields
                            (get k nil)
                            (util/as-double)))
            ]
        (assoc candidate
               ;; scaling factors applied here to match the units in the UI
               ::supply/capacity-kwp  supply-capacity-kw
               ::supply/capacity-kwh  (nil-field SUPPLY-CAPACITY-KWH-FIELD)
               ::supply/cost-per-kwh  (field SUPPLY-KWH-COST-FIELD)
               ::supply/capex-per-kwp (field SUPPLY-KWP-CAPEX-FIELD)
               ::supply/opex-per-kwp  (field SUPPLY-KWP-COST-FIELD)
               ::supply/opex-fixed    (field SUPPLY-FIXED-OPEX-FIELD)
               ::supply/fixed-cost    (field SUPPLY-FIXED-CAPEX-FIELD)
               ::supply/emissions
               {:co2                  (/ (field SUPPLY-CO2-FIELD)  1000.0)
                :nox                  (/ (field SUPPLY-NOX-FIELD)  1000000.0)
                :pm25                 (/ (field SUPPLY-PM25-FIELD) 1000000.0)
                })))))



;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-specs.supply
  (:require [clojure.spec.alpha :as s]
            [spec-tools.data-spec :as ds]
            [thermos-util :as util]
            [com.rpl.specter :as sr]))

;; These are the "network" supply parameters which control the cost /
;; etc of heat supply in the network model

(s/def ::supply
  (s/keys :req [::capacity-kwp
                ::cost-per-kwh
                ::capex-per-kwp
                ::opex-per-kwp
                ::fixed-cost
                ::emissions]))

(s/def ::alternative
  (s/keys :req [::id ::name
                ::cost-per-kwh
                ::capex-per-kwp
                ::capex-per-mean-kw
                ::opex-per-kwp
                ::fixed-cost
                ::emissions

                ;; if this field contains a number, then the network model
                ;; will be told a lie about the cost/kwp for this alternative
                ::kwp-per-mean-kw
                ]))

(defn principal [candidate capacity-kw annual-kwh]
  (+ (::fixed-cost candidate 0)
     (* (or capacity-kw 0)
        (::capex-per-kwp candidate 0))
     (* (util/annual-kwh->kw (or annual-kwh 0))
        (::capex-per-mean-kw candidate 0))))

(defn opex [candidate capacity-kw]
  (* (or capacity-kw 0) (::opex-per-kwp candidate 0)))

(defn heat-cost [candidate consumption-kwh]
  (* (or consumption-kwh 0) (::cost-per-kwh candidate 0)))

;; However, we also have supply parameters for the plant model
;; These live in the document at the top level

(defn- matches-day-types [path]
  (fn [d]
    (let [day-types (::day-types d)
          values    (sr/select path d); [{day type => values}]
          day-type-keys (set (keys day-types))
          ]
      ;; the keys for each entry in values should be exactly right
      (every? (partial = day-type-keys)
              (map (comp set keys) values)))))

(defn- matches-divisions [path]
  (fn [d]
    (let [day-types (::day-types d)
          values    (sr/select path d); [{day type => values}]
          ]
      ;; the keys for each entry in values should be exactly right
      (every? (fn [[day-type values]]
                (let [desired (:divisions (get day-types day-type) :invalid)
                      actual (count values)]
                  (println desired actual)
                  (= desired actual)))
              (apply concat values)))))

(s/def ::supply-problem
  (s/and
   (s/keys :req [::day-types ::heat-profiles ::plants ::storages ::substations ::fuels])

   (matches-day-types [::heat-profiles sr/MAP-VALS :demand])
   (matches-day-types [::fuels sr/MAP-VALS :price])
   (matches-day-types [::fuels sr/MAP-VALS :co2])
   (matches-day-types [::fuels sr/MAP-VALS :nox])
   (matches-day-types [::fuels sr/MAP-VALS :pm25])
   
   (matches-divisions [::heat-profiles sr/MAP-VALS :demand])
   (matches-divisions [::fuels sr/MAP-VALS :price])
   (matches-divisions [::fuels sr/MAP-VALS :co2])
   (matches-divisions [::fuels sr/MAP-VALS :nox])
   (matches-divisions [::fuels sr/MAP-VALS :pm25])

   
   ))

(s/def ::fuel-id int?)
(s/def ::plant-id int?)
(s/def ::storage-id int?)
(s/def ::substation-id int?)
(s/def ::profile-id int?)
(s/def ::day-type-id int?)

(s/def ::day-type
  (ds/spec
   ::day-type
   {:name string?
    :frequency int?
    :divisions int?}))

(s/def ::day-types
  (s/map-of ::day-type-id ::day-type))

(def day-values {(s/spec ::day-type-id) [number?]})

(s/def ::heat-profile
  (ds/spec
   ::heat-profile
   {:name string?
    :demand day-values}))

(s/def ::heat-profiles
  (s/map-of ::profile-id ::heat-profile))

(s/def ::fuel
  (ds/spec
   ::fuel
   {:name string?
    :price day-values
    :co2   day-values
    :pm25  day-values
    :nox   day-values}))

(s/def ::fuels
  (s/map-of ::fuel-id ::fuel))

(s/def ::plant
  (ds/spec
   ::plant
   {:name string?
    :fuel ::fuel-id
    :chp boolean?
    :capacity-kwp double?
    :power-efficiency (ds/maybe double?)
    :heat-efficiency double?
    :substation (ds/maybe ::substation-id)
    :lifetime pos-int?
    :capital-cost   any?
    :operating-cost any?}))

(s/def ::plants (s/map-of ::plant-id ::plant))


(s/def ::storage
  (ds/spec
   ::storage
   {:name string?
    :capital-cost any?
    :lifetime pos-int?
    :capacity-kwh double?
    :efficiency double?}))

(s/def ::storages
  (s/map-of ::storage-id ::storage))

(s/def ::substation
  (ds/spec
   ::substation
   {:name string?
    :headroom-kwp double?
    :alpha double?}))

(s/def ::substations
  (s/map-of ::substation-id ::substation))

(defn remove-fuel [d fuel-id]
  (sr/multi-transform
   (sr/multi-path
    [::fuels fuel-id (sr/terminal-val sr/NONE)]
    [::plants sr/MAP-VALS :fuel (sr/pred= fuel-id) (sr/terminal-val nil)])
   d))

(defn remove-substation [d sub-id]
  (sr/multi-transform
   (sr/multi-path
    [::substations sub-id (sr/terminal-val sr/NONE)]
    [::plants sr/MAP-VALS :substation (sr/pred= sub-id) (sr/terminal-val nil)])
   d))

(defn remove-plant [d plant-id]
  (update d ::plants dissoc plant-id))

(defn remove-storage [d storage-id]
  (update d ::storages dissoc storage-id))

(defn remove-profile [d profile-id]
  (-> d
      (update ::heat-profiles dissoc profile-id)
      (->> (sr/setval [:thermos-specs.document/candidates
                       sr/MAP-VALS
                       ::profile-id
                       (sr/pred= profile-id)]
                      sr/NONE)
           (sr/setval [::default-profile (sr/pred= profile-id)]
                      sr/NONE))))

(defn remove-day-type [d day-type-id]
  (sr/multi-transform
   (sr/multi-path
    [::day-types day-type-id (sr/terminal-val sr/NONE)]
    [::grid-offer day-type-id (sr/terminal-val sr/NONE)]
    [::heat-profiles sr/MAP-VALS :demand day-type-id (sr/terminal-val sr/NONE)]
    [::fuels sr/MAP-VALS (sr/multi-path :price :co2 :nox :pm25)
     day-type-id (sr/terminal-val sr/NONE)])
   d))

(defn- next-id [m]
  (inc (reduce max -1 (keys m))))

(defn- assoc-id [m v]
  (assoc m (next-id m) v))

(defn add-day-type [d day-type]
  {:pre [(s/valid? ::day-type day-type)]}
  (update d ::day-types assoc-id day-type))

(defn add-profile [d profile]
  {:pre [(s/valid? ::heat-profile profile)]}
  (update d ::heat-profiles assoc-id profile))

(defn add-storage [d storage]
  {:pre [(s/valid? ::storage storage)]}
  (update d ::storages assoc-id storage))

(defn add-plant [d plant]
  {:pre [(s/valid? ::plant plant)]}
  (update d ::plants assoc-id plant))

(defn add-fuel [d fuel]
  {:pre [(s/valid? ::fuel fuel)]}
  (update d ::fuels assoc-id fuel))

(defn add-substation [d substation]
  {:pre [(s/valid? ::substation substation)]}
  (update d ::substations assoc-id substation))

(defn reinterpolate
  "Given `values`, a vector of numbers, resize it to have `new-divs` length.

  The resized vector should have the same AUC."
  [new-divs values]
  (cond
    (= new-divs (count values)) values ;; noop
    ;; else do something

    (apply = values)
    (vec (repeat new-divs (or (first values) 0)))

    :else
    ;; preserve AUC
    (let [width       (double (/ new-divs))
          old-count   (count values)
          old-width   (double (/ old-count))
          width-ratio (/ old-width width)
          ]
      (vec
       (for [i (range new-divs)]
         (let [;; these are the scaled x-values
               x0 (* i width)
               x1 (* (inc i) width)
               ;; so these are the (perhaps fractional) indices
               ;; covered in the old data
               i0 (* x0 old-count)
               i1 (* x1 old-count)

               i0-floor (int (Math/floor i0))
               i0-ceil  (int (Math/ceil i0))
               i1-floor (int (Math/floor i1))

               left-bit  (* (get values i0-floor) (- i0 i0-floor))
               covered   (if (> i1-floor i0-ceil)
                           (reduce + (subvec values i0-ceil i1-floor))
                           0)
               delta     (- i1 i1-floor)
               right-bit (if (zero? delta) 0 (* (get values i1-floor) delta))
               ]
           (* width-ratio (+ left-bit covered right-bit))))))))

(defn change-divisions [d day-type divs]
  (sr/multi-transform
   (sr/multi-path
    [::day-types day-type :divisions (sr/terminal-val divs)]
    [::heat-profiles sr/MAP-VALS :demand day-type (sr/putval divs) (sr/terminal reinterpolate)]
    [::grid-offer day-type (sr/putval divs) (sr/terminal reinterpolate)]
    [::fuels sr/MAP-VALS (sr/multi-path :price :co2 :nox :pm25) day-type
     (sr/putval divs) (sr/terminal reinterpolate)])
   d))


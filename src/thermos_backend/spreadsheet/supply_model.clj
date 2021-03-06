;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.spreadsheet.supply-model
  (:require [thermos-backend.spreadsheet.common :as sheet]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.solution :as solution]
            [thermos-specs.supply :as supply]
            [clojure.string :as string]
            [clojure.set :as set]
            [com.rpl.specter :as S]))

(defn *100 [x] (and x (* 100.0 x)))
(defn ÷100 [x] (and x (/ x 100.0)))

(defn output-solution [ss doc]
  ss)

(defn- lkup [by-day-by-interval [day interval]]
  (let [by-interval (get by-day-by-interval day)]
    (cond
      (vector? by-interval)
      (nth by-interval interval)
      (map? by-interval)
      (get by-interval interval)
      true 0)))

(defn output-profiles [ss doc]
  (let [days*divisions
        (for [[day-id day-type] (::supply/day-types doc)
              :let              [dn (:name day-type)]
              division          (range (:divisions day-type))]
          [day-id division dn])]
    (as-> ss ss
      (sheet/add-tab
       ss
       "Supply day types"
       [{:name "Name" :key :name}
        {:name "Divisions" :key :divisions}
        {:name "Frequency" :key :frequency}]
       (vals (::supply/day-types doc)))

      (sheet/add-tab
       ss
       "Supply profiles"
       (into [{:name "Day type" :key #(nth % 2)}
              {:name "Interval" :key second}
              {:name "Grid offer"
               :key  (partial lkup (::supply/grid-offer doc))}]
             
             `[~@(for [[_ {:keys [name price co2 nox pm25]}] (::supply/fuels doc)
                       [sub-name series]                     [["price" price]
                                          [(candidate/text-emissions-labels :co2) co2]
                                          [(candidate/text-emissions-labels :nox) nox]
                                          [(candidate/text-emissions-labels :pm25) pm25]]
                       
                       ]
                   {:name (str sheet/fuel-profile-prefix name " " sub-name) :key (partial lkup series)})

               ~@(for [[_ {:keys [name demand]}] (::supply/heat-profiles doc)]
                   {:name (str sheet/profile-prefix name) :key (partial lkup demand)})

               ~@(for [[_ {:keys [name load-kw]}] (::supply/substations doc)]
                   {:name (str sheet/substation-profile-prefix name) :key (partial lkup load-kw)})])
       days*divisions)

      (if (and (::solution/supply-solution doc)
               (::solution/supply-problem doc))
        (let [problem  (::solution/supply-problem doc)
              solution (::solution/supply-solution doc)
              profile  (:profile problem)]
          (sheet/add-tab
           ss
           "Supply operations"
           `[~{:name "Day type" :key #(nth % 2)}
             ~{:name "Interval" :key second}
             ~{:name "Heat load"
               :key
               (fn [[day interval]]
                 (nth (:heat-demand (get profile day)) interval))}

             ~@(for [[plant-id {:keys [build input output generation]}] (:plant solution)
                     :when build
                     :let [plant-name (:name (get (::supply/plants doc) plant-id))]
                     [label values]
                     (cond-> [["fuel input" input] ["heat output" output]]
                       generation (conj [["grid export" generation]]))]
                 
                 {:name (str plant-name " " label)
                  :key (partial lkup values)}
                 )

              ~@(for [[storage-id {:keys [input output]}] (:storage solution)
                      :let [storage-name (:name (get (::supply/storages doc) storage-id))]
                      [label values] [["heat input" input] ["heat output" output]]]
                  {:name (str storage-name " " label)
                   :key (partial lkup values)})

             ~{:name "Curtailment"
               :key (partial lkup (:curtailment solution))}
             ]
           days*divisions))
        ss)

      (sheet/add-tab
       ss
       "Supply parameters"
       [{:name "Parameter" :key first}
        {:name "Value" :key second}]

       `[~["Accounting Period (years)" (get-in doc [::supply/objective :accounting-period])]
         ~["Discount Rate" (get-in doc [::supply/objective :discount-rate])]
         ~["Curtailment Cost (¤/kWh)" (get-in doc [::supply/objective :curtailment-cost])]
         ~["Can Dump Heat" (get-in doc [::supply/objective :can-dump-heat])]
         ~["MIP Gap" (get-in doc [::supply/objective :mip-gap])]
         ~["Time Limit (hours)" (get-in doc [::supply/objective :time-limit])]
         ~["Default profile" (get-in doc [::supply/heat-profiles (::supply/default-profile doc) :name])]]))))

(defn output-technologies [ss doc]
  (let [fuels             (::supply/fuels doc)
        substations       (::supply/substations doc)
        input-plants      (::supply/plants doc)
        input-storage     (::supply/storages doc)
        
        solution          (::solution/supply-solution doc)
        plant-decisions   (:plant solution)
        storage-decisions (:storage solution)]
    (-> ss
        (sheet/add-tab
         "Supply Plant"
         (into
          [{:name "Name" :key (comp :name second)}
           {:name "Lifetime" :key (comp :lifetime second)}
           {:name "Fuel" :key (comp :name fuels :fuel second)}
           {:name "Capacity" :key (comp :capacity-kwp second)}
           {:name "CHP?" :key (comp :chp second)}
           {:name "Heat Efficiency" :key (comp *100 :heat-efficiency second)}
           {:name "Power Efficiency" :key (comp *100 :power-efficiency second)}
           {:name "Fixed Capex" :key (comp :fixed :capital-cost second)}
           {:name "Capex/kWp" :key (comp :per-kwp :capital-cost second)}
           {:name "Capex/kWh" :key (comp :per-kwh :capital-cost second)}
           {:name "Fixed Opex" :key (comp :fixed  :operating-cost second)}
           {:name "Opex/kWp" :key  (comp :per-kwp :operating-cost second)}
           {:name "Opex/kWh" :key  (comp :per-kwh :operating-cost second)}
           {:name "Substation" :key (comp :name substations :substation second)}]

          (when solution
            `[~{:name "Build?" :key (comp :build plant-decisions first)}
              ~{:name "Built capacity" :key (comp :capacity-kw plant-decisions first)}
              ~{:name "Output kWh/yr" :key (comp :output-kwh plant-decisions first)}
              ~@(for [k [:lifetime-cost :present-cost :total-cost]]
                  {:name (string/capitalize (.replace (name k) "-" " "))
                   :key  (comp k :capital-cost plant-decisions first)})
              ~@(for [k [:annual-cost :present-cost :total-cost]
                      f [:operating-cost :fuel-cost :grid-revenue]
                      ]
                  {:name
                   (str
                    (string/capitalize (.replace (name f) "-" " "))
                    " "
                    (string/capitalize (.replace (name k) "-" " ")))
                   :key  (comp k f plant-decisions first)})
              ~@(for [k [:annual-cost :present-cost :total-cost
                         :annual-emission :total-emission]
                      f [:co2 :nox :pm25]
                      ]
                  {:name
                   (str
                    (candidate/text-emissions-labels f)
                    " "
                    (string/capitalize (.replace (name k) "-" " ")))
                   :key  (comp k f :emissions plant-decisions first)})
             ]
            )
          )
         input-plants
         )

        (sheet/add-tab
         "Supply Storage"
         (into
          [{:name "Name" :key (comp :name second)}
           {:name "Lifetime" :key (comp :lifetime second)}
           {:name "Capacity (kWh)" :key (comp :capacity-kwh second)}
           {:name "Capacity (kWp)" :key (comp :capacity-kwp second)}
           {:name "Efficiency" :key (comp *100 :efficiency second)}
           {:name "Fixed Capex" :key (comp :fixed :capital-cost second)}
           {:name "Capex/kWp" :key (comp :per-kwp :capital-cost second)}
           {:name "Capex/kWh" :key (comp :per-kwh :capital-cost second)}
           ]
          (when solution
            `[~{:name "Build capacity (kWh)" :key (comp :capacity-kwh storage-decisions first)}
              ~{:name "Build capacity (kWp)" :key (comp :capacity-kw storage-decisions first)}
              ~@(for [k [:lifetime-cost :present-cost :total-cost]]
                  {:name (string/capitalize (.replace (name k) "-" " "))
                   :key  (comp k :capital-cost storage-decisions first)}
                  )
             ]
            )
          )
         input-storage)

        (sheet/add-tab
         "Supply Substations"
         (into
          [{:name "Name" :key (comp :name second)}
           {:name "Headroom (kWp)" :key (comp :headroom-kwp second)}
           {:name "Alpha" :key (comp :alpha second)}])
         substations)
        )))

(defn id-lookup
  "Transforms maps of shape {0 {:name A}, 1 {:name B}} to {A 0, B 1}"
  [m & {:keys [key-field] :or {key-field :name}}]
  (set/map-invert (S/transform [S/MAP-VALS] key-field m)))

(defn- read-day-types
  "Read the supply day types tab from the input spreadsheet."
  [spreadsheet]
  (-> (for [{:keys [name
                    divisions
                    frequency]}
            (:rows (:supply-day-types spreadsheet))]
        {:name name
         :divisions divisions
         :frequency frequency})
      (sheet/index)))

(defn- read-substations [spreadsheet profiles]
  (-> (for [{:keys [name
                    headroom-kwp
                    alpha]}
            (:rows (:supply-substations spreadsheet))]
        (let [load-profile-id (sheet/to-keyword (str sheet/substation-profile-prefix name))]
          {:name name
           :headroom-kwp headroom-kwp
           :alpha alpha
           :load-kw (load-profile-id profiles)}))
      (sheet/index)))

(defn- read-fuels [profiles profile-names]
  (let [fuels (-> (for [profile (keys profiles)
                        :when (string/starts-with? (name profile) "fuel-")]
                    (string/replace (name profile) #"-price$|-nox$|-co2$|-pm25$" ""))
                  (set))]
    (-> (for [fuel fuels]
          {:name  (string/replace ((keyword (str fuel "-price")) profile-names) #"^Fuel: | price$" "")
           :price ((keyword (str fuel "-price")) profiles)
           :co2   ((keyword (str fuel "-co2")) profiles)
           :pm25  ((keyword (str fuel "-pm25")) profiles)
           :nox   ((keyword (str fuel "-nox")) profiles)})
        (sheet/index))))

(defn- read-profiles
  "Read the profiles tab from the input spreadsheet and convert it
   into a map of profiles in the internal format."
  [spreadsheet]
  (let [{:keys [supply-profiles]} spreadsheet

        day-type-to-id (id-lookup (read-day-types spreadsheet))

        profile-names (dissoc (:header supply-profiles) :day-type :interval)]
    (->> (:rows supply-profiles)
         (sort-by (juxt :day-type :interval))
         (map (fn [row]
                (for [name (keys profile-names)]
                  {:name name
                   :day-type (day-type-to-id (:day-type row))
                   :interval (:interval row)
                   :val (name row)})))
         (flatten)
         (group-by :name)
         (S/transform [S/MAP-VALS] (fn [e] (group-by :day-type e)))
         (S/transform [S/MAP-VALS S/MAP-VALS S/ALL] :val))))

(defn- read-heat-profiles [profiles profile-names]
  (let [profiles (->> profiles
                      (filter (fn [[k _]] (string/starts-with? (name k) "profile-")))
                      (into {}))]
    (-> (for [[profile-name profile] profiles]
          {:name (sheet/without-profile-prefix (profile-name profile-names))
           :demand profile})
        (sheet/index))))

(defn input-from-spreadsheet
  "Inverse function - takes a spreadsheet, as loaded by `common/read-to-tables`.

   Assumes validation has happened already."
  [spreadsheet]
  (let [{:keys [supply-plant
                supply-profiles
                supply-storage
                supply-parameters]} spreadsheet

        parameters
        (reduce
         (fn [a {:keys [parameter value]}]
           (assoc a (sheet/to-keyword parameter) value))
         {} (:rows supply-parameters))

        profile-names (dissoc (:header supply-profiles) :day-type :interval)
        profiles (read-profiles spreadsheet)

        substations (read-substations spreadsheet profiles)
        substation-ids (id-lookup substations)

        fuels (read-fuels profiles profile-names)
        fuel-ids (id-lookup fuels)
        
        heat-profiles (read-heat-profiles profiles profile-names)]
    (merge
     {::supply/plants
      (-> (for [{:keys [fixed-capex capex-per-kwp capex-per-kwh
                        name
                        capacity
                        fuel
                        heat-efficiency
                        power-efficiency
                        substation
                        fixed-opex opex-per-kwp opex-per-kwh
                        chp?
                        lifetime]}
                (:rows supply-plant)]
            {:capital-cost {:fixed fixed-capex, :per-kwp capex-per-kwp, :per-kwh capex-per-kwh}
             :name name
             :capacity-kwp capacity
             :fuel (get fuel-ids fuel)
             :heat-efficiency (÷100 heat-efficiency)
             :power-efficiency (÷100 power-efficiency)
             :substation (get substation-ids substation)
             :operating-cost {:fixed fixed-opex, :per-kwp opex-per-kwp, :per-kwh opex-per-kwh}
             :chp chp?
             :lifetime lifetime})
          (sheet/index))

      ::supply/day-types
      (read-day-types spreadsheet)

      ::supply/storages
      (-> (for [{:keys [name
                        capacity-kwh
                        capacity-kwp
                        efficiency
                        fixed-capex capex-per-kwp capex-per-kwh
                        lifetime]}
                (:rows supply-storage)]
            {:name name
             :capacity-kwh capacity-kwh
             :capacity-kwp capacity-kwp
             :efficiency (÷100 efficiency)
             :capital-cost {:fixed fixed-capex, :per-kwp capex-per-kwp, :per-kwh capex-per-kwh}
             :lifetime lifetime})
          (sheet/index))

      ::supply/heat-profiles
      heat-profiles

      ::supply/grid-offer
      (:grid-offer profiles)

      ::supply/fuels
      fuels

      ::supply/substations
      substations

      ::supply/objective
      (select-keys parameters [:accounting-period
                               :discount-rate
                               :curtailment-cost
                               :can-dump-heat
                               :mip-gap
                               :time-limit])
      
      ::supply/default-profile
      (let [default-name (:default-profile parameters)]
        (get (id-lookup heat-profiles) default-name))})))

(defn output-problem [ss doc]
  (-> ss
      (output-technologies doc)
      (output-profiles doc)
      ))

(defn output-to-spreadsheet [ss doc]
  (-> ss
    (output-problem doc)
    ))


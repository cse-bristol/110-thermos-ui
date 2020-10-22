(ns thermos-backend.spreadsheet.supply-model
  (:require [thermos-backend.spreadsheet.common :as sheet]
            [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.tariff :as tariff]
            [thermos-specs.solution :as solution]
            [thermos-specs.path :as path]
            [thermos-specs.supply :as supply]
            [thermos-util.pipes :as pipes]
            [thermos-specs.measure :as measure]
            [clojure.string :as string]))

(defn *100 [x] (and x (* 100.0 x)))

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
                   {:name (str name " " sub-name) :key (partial lkup series)})

               ~@(for [[_ {:keys [name demand]}] (::supply/heat-profiles doc)]
                   {:name (str "Profile: " name) :key (partial lkup demand)})

               ~@(for [[_ {:keys [name load-kw]}] (::supply/substations doc)]
                   {:name (str "Substation: " name) :key (partial lkup load-kw)})])
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
      )))

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

        )))

(defn output-problem [ss doc]
  (-> ss
      (output-technologies doc)
      (output-profiles doc)
      ))

(defn output-to-spreadsheet [ss doc]
  (-> ss
    (output-problem doc)
    ))


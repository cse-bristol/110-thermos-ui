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
            [thermos-specs.measure :as measure]))


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


(defn output-problem [ss doc]
  (-> ss
      (output-profiles doc)
      ))

(defn output-to-spreadsheet [ss doc]
  (-> ss
    (output-problem doc)
    ))


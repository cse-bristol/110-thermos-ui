;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.spreadsheet.network-model
  "Functions to output network model data into a spreadsheet"
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
            [clojure.set :as set]
            [thermos-backend.spreadsheet.common :as common]
            [thermos-util.solution-summary :as solution-summary]))

(def *100 (partial * 100.0))
(defn or-zero [f] (fn [x] (or (f x) 0.0)))

(defn cost-columns [type prefix key]
  (case type
    :capex
    [{:name (str prefix " (principal, ¤)") :key (comp :principal key)}
     {:name (str prefix " (NPV, ¤)") :key (comp :present key)}
     {:name (str prefix " (total, ¤)") :key (comp :total key)}]
    
    :opex
    [{:name (str prefix " (¤/yr)") :key (comp :annual key)}
     {:name (str prefix " (NPV, ¤)") :key (comp :present key)}
     {:name (str prefix " (total, ¤)") :key (comp :total key)}]
    
    :emission
    [{:name (str prefix " (kg/yr)") :key (comp :kg key)}
     {:name (str prefix " (¤/yr)") :key (comp :annual key)}
     {:name (str prefix " (NPV ¤)") :key (comp :present key)}
     {:name (str prefix " (total ¤)") :key (comp :total key)}]
    )
  
  )

(defn candidate-columns [doc]
  (into [{ :name "ID"                  :key ::candidate/id}
         { :name "Constraint"  :key (comp name ::candidate/inclusion) }]
        (for [user-field
              (sort (set (mapcat (comp keys ::candidate/user-fields)
                                 (vals (::document/candidates doc)))))]

          {:name user-field
           :key (fn [x] (get (::candidate/user-fields x) user-field))})))

(defn building-columns [doc]
  (let [mode (document/mode doc)
        peak-demand   #(candidate/solved-peak-demand % mode)
        annual-demand #(candidate/solved-annual-demand % mode)
        ]
    (concat
     (candidate-columns doc)
     [{ :name "Annual demand (kWh)" :key annual-demand        :format :double}
      { :name "Peak demand (kW)"    :key peak-demand          :format :double}
      { :name "Demand model"        :key ::demand/source}
      { :name "Tariff name"
       :key #(let [name (document/tariff-name doc (::tariff/id %))
                   is-market (= :market (::tariff/id %))
                   has-market-value (::solution/market-rate %)]
               (if (and is-market has-market-value)
                 (str name " (" (*100 has-market-value) " c/kWh)")
                 name))}
      { :name "Connection cost name"
       :key #(document/connection-cost-name doc (::tariff/cc-id %))}
      ])))

(defn output-buildings [ss doc]
  (let [buildings (filter candidate/is-building? (vals (::document/candidates doc)))
        base-cols (building-columns doc)]
    (if (document/has-solution? doc)
      (let [networked-buildings (filter candidate/is-connected? buildings)
            non-networked-buildings (remove candidate/is-connected? buildings)
            supply-buildings (filter candidate/has-supply? buildings)
            ]
        (-> ss
            (sheet/add-tab
             "Networked buildings"
             (concat
              base-cols
              (cost-columns :opex "Revenue" ::solution/heat-revenue)
              (cost-columns :capex "Connection cost" ::solution/connection-capex))
             networked-buildings)
            
            (sheet/add-tab
             "Other buildings"
             (concat
              base-cols
              [{:name "Heating system"  :key candidate/solution-description}]
              (cost-columns :capex "System capex" (comp :capex ::solution/alternative))
              (cost-columns :opex "System opex" (comp :opex ::solution/alternative))
              (cost-columns :opex "System fuel" (comp :heat-cost ::solution/alternative))


              (apply concat
                     (for [e candidate/emissions-types]
                       (cost-columns
                        :emission
                        (candidate/text-emissions-labels e)
                        (comp e :emissions ::solution/alternative)))))
             
             non-networked-buildings
             )

            (sheet/add-tab
             "Supply points"
             (concat
              base-cols
              [{:name "Maximum capacity (kW)" :key ::supply/capacity-kwp}
               {:name "Heat price (c/kWh)" :key (comp *100 ::supply/cost-per-kwh)}

               {:name "Fixed capital cost (¤)" :key ::supply/fixed-cost}
               {:name "Capital cost (¤/kWp)" :key ::supply/capex-per-kwp}
               {:name "Operating cost (¤/kWp)" :key ::supply/opex-per-kwp}

               {:name "Installed capacity (kW)" :key ::solution/capacity-kw}
               {:name "Coincidence" :key ::solution/diversity}
               {:name "Annual output (kWh/yr)" :key ::solution/output-kwh}
               ]

              (cost-columns :capex "Plant capex" ::solution/supply-capex)
              (cost-columns :opex "Heat cost" ::solution/heat-cost)

              (apply concat
                     (for [e candidate/emissions-types]
                       (cost-columns
                        :emission
                        (str "Plant " (candidate/text-emissions-labels e))
                        (comp e ::solution/supply-emissions))))

              [{:name "Pumping kWh" :key ::solution/pumping-kwh}]
              (cost-columns :opex "Pumping cost" ::solution/pumping-cost)
              
              (apply concat
                     (for [e candidate/emissions-types]
                       (cost-columns
                        :emission
                        (str "Pumping " (candidate/text-emissions-labels e))
                        (comp e ::solution/pumping-emissions))))
              )
             supply-buildings)
            ))
      
      (sheet/add-tab
       ss "Buildings"
       base-cols
       buildings)
      )))

(defn output-pipe-costs [ss doc]
  (let [pipe-costs (::document/pipe-costs doc)
        civils     (:civils pipe-costs)
        curve-rows (pipes/curve-rows (pipes/curves doc))
        ]
    (sheet/add-tab
     ss
     "Pipe costs"
     (into
      [{:name "NB (mm)" :key (comp (partial * 1000.0) :diameter)}
       {:name "Capacity (kW)" :key :capacity-kw}
       {:name "Losses (kWh/m.yr)" :key :losses-kwh}
       {:name "Pipe cost (¤/m)" :key :pipe}]
      
      (for [[id n] civils]
        {:name (str n " (¤/m)") :key #(get % id)}))

     curve-rows
     )))


(def capital-cost-names
  "Used to output keys from ::document/capital-costs"
  {:alternative "Other heating"
   :connection "Connections"
   :insulation "Insulation"
   :supply "Supply"
   :pipework "Pipework"})

(defn output-network-parameters [ss doc]
  (-> ss
      (sheet/add-tab
       "Tariffs"
       [{:name "Tariff name" :key ::tariff/name}
        {:name "Unit rate (c/¤)" :key (comp *100 (or-zero ::tariff/unit-charge))}
        {:name "Capacity charge (¤/kWp)" :key (or-zero ::tariff/capacity-charge)}
        {:name "Standing charge (¤)" :key (or-zero ::tariff/standing-charge)}]
       (vals (::document/tariffs doc)))
      
      (sheet/add-tab
       "Connection costs"
       [{:name "Cost name" :key ::tariff/name}
        {:name "Fixed cost (¤)" :key        (or-zero ::tariff/fixed-connection-cost)}
        {:name "Capacity cost (¤/kWp)" :key (or-zero ::tariff/variable-connection-cost)}]
       (vals (::document/connection-costs doc)))

      (sheet/add-tab
       "Individual systems"
       (into
        [{:name "Name" :key ::supply/name}
         {:name "Fixed cost (¤)" :key (or-zero ::supply/fixed-cost)}
         {:name "Capacity cost (¤/kWp)" :key (or-zero ::supply/capex-per-kwp)}
         {:name "Operating cost (¤/kWp.yr)" :key (or-zero ::supply/opex-per-kwp)}
         {:name "Heat price (c/kWh)" :key (comp *100 (or-zero ::supply/cost-per-kwh))}
         {:name "Tank factor" :key ::supply/kwp-per-mean-kw}
         ]
        (for [e candidate/emissions-types]
          {:name (str (candidate/text-emissions-labels e)
                      " ("
                      (candidate/emissions-factor-units e)
                      ")")
           :key (comp (partial * (candidate/emissions-factor-scales e))
                      #(e % 0) ::supply/emissions)}
          ))
       
       (vals (::document/alternatives doc)))

      (sheet/add-tab
       "Insulation"
       [{:name "Name" :key ::measure/name}
        {:name "Fixed cost" :key (or-zero ::measure/fixed-cost)}
        {:name "Cost / m2" :key (or-zero ::measure/cost-per-m2)}
        {:name "Maximum reduction %" :key (comp *100 (or-zero ::measure/maximum-effect))}
        {:name "Maximum area %" :key (comp *100 (or-zero ::measure/maximum-area))}
        {:name "Surface" :key (comp name ::measure/surface)}]
       (vals (::document/insulation doc)))
      
      
      (output-pipe-costs doc)

      (sheet/add-tab
       "Capital costs"
       [{:name "Name" :key (comp capital-cost-names first)}
        {:name "Annualize" :key (comp boolean :annualize second)}
        {:name "Recur" :key (comp boolean :recur second)}
        {:name "Period (years)" :key #(int (:period (second %) 0))}
        {:name "Rate (%)" :key #(* 100 (:rate (second %) 0))}]

       (::document/capital-costs doc))
      
      
      (sheet/add-tab
       "Other parameters"
       [{:name "Parameter" :key first}
        {:name "Value" :key second}]

       `[~["Medium" (name (document/medium doc))]

         ~@(if (= :hot-water (document/medium doc))
             [["Flow temperature"   (::document/flow-temperature doc)]
              ["Return temperature"   (::document/return-temperature doc)]
              ["Ground temperature"   (::document/ground-temperature doc)]]
             [["Steam pressure (MPa)" (document/steam-pressure doc)]
              ["Steam velocity (m/s)" (document/steam-velocity doc)]])

          ~["Pumping overhead" (*100 (::document/pumping-overhead doc 0))]
          ~["Pumping cost/kWh" (::document/pumping-cost-per-kwh doc 0)]

         ~@(for [[e f] (::document/pumping-emissions doc)]
             [(str "Pumping "
                   (candidate/text-emissions-labels e)
                   " ("
                   (candidate/emissions-factor-units e)
                   ")")
              (* (or f 0) (candidate/emissions-factor-scales e))])

         ~@(let [prices (::document/emissions-cost doc)]
             (for [e candidate/emissions-types
                   :let [cost (get prices e)]
                   :when cost]
               [(str (candidate/text-emissions-labels e) " cost") cost]))

         ~@(let [limits (::document/emissions-limit doc)]
             (for [e candidate/emissions-types
                   :let [{:keys [value enabled]} (get limits e )]]
               [(str (candidate/text-emissions-labels e) " limit")
                (if enabled value "none")]))
         
         ~["Objective" (name (::document/objective doc))]
         ~["Consider alternative systems" (::document/consider-alternatives doc)]
         ~["Consider insulation" (::document/consider-insulation doc)]
         
         ~["NPV Term" (::document/npv-term doc)]
         ~["NPV Rate" (*100 (::document/npv-rate doc))]

         ~["Loan Term" (::document/loan-term doc)]
         ~["Loan Rate" (*100 (::document/loan-rate doc))]
         
         ~["MIP Gap" (::document/mip-gap doc)]
         ~["Param Gap" (::document/param-gap doc)]
         ~["Max runtime" (::document/maximum-runtime doc)]

         ~["Max supplies" (::document/maximum-supply-sites
                            doc
                            "unlimited")]
         ]
       )
      
      )
  )

(defn output-paths [ss doc]
  (sheet/add-tab
   ss "Paths & pipes"
   (concat
    (candidate-columns doc)
    [{ :name "Length"              :key ::path/length}
     { :name "Civils"   :key #(document/civil-cost-name doc (::path/civil-cost-id %)) }
     { :name "Exists"    :key ::path/exists }]
    (when (document/has-solution? doc)
      (concat
       [{:name "In solution" :key ::solution/included}
        {:name "Diameter (mm)" :key ::solution/diameter-mm}
        {:name "Capacity (kW)" :key ::solution/capacity-kw}
        {:name "Losses (kWh/yr)" :key ::solution/losses-kwh}
        {:name "Coincidence" :key ::solution/diversity}

        ]
       (cost-columns :capex "Capital cost" ::solution/pipe-capex)
       )))
   (filter candidate/is-path? (vals (::document/candidates doc)))))
             
(defn output-solution-summary [ss doc]
  (if (document/has-solution? doc)
    (let [{:keys [rows grand-total]} (solution-summary/data-table doc :total :total)]
      (sheet/add-tab
       ss
       "Network solution summary"
       [{:name "Item" :key :name}
        {:name "Capital cost (¤)" :key :capex}
        {:name "Operating cost (¤)" :key :opex}
        {:name "Operating revenue (¤)" :key :revenue}
        {:name "EC (c/kWh)" :key :equivalized-cost}
        {:name "NPV (¤)" :key :present}]
       (flatten
        (concat
         (for [{:keys [name subcategories total]} rows]
           (concat
            (for [{sub-name :name value :value} subcategories]
              (assoc value :name sub-name))
            [(assoc total :name name)]
            [{}])) ; Add an empty row between categories
         
         [{:name "Whole system"
           :capex (:capex grand-total)
           :opex (:opex grand-total)
           :present (:present grand-total)}]))))
    ss))

(defn output-to-spreadsheet [ss doc]
  (-> ss
      (output-buildings doc)
      (output-paths doc)
      (output-network-parameters doc)
      (output-solution-summary doc)
      ))

(defn input-from-spreadsheet
  "Inverse function - takes a spreadsheet, as loaded by `common/read-to-tables`.

   So far this ignores everything to do with candidates.

   Assumes validation has happened already."
  [ss]
  (let [{:keys [tariffs
                connection-costs
                individual-systems
                pipe-costs
                insulation
                other-parameters
                capital-costs]} ss

        parameters
        (reduce
         (fn [a {:keys [parameter value]}]
           (assoc a (common/to-keyword parameter) value))
         {} (:rows other-parameters))

        copy-parameter
        (fn [out-key in-key & {:keys [type convert]
                               :or {type number? convert identity}}]
          (when-let [v (get parameters out-key)] (when (type v) {in-key (convert v)})))

        bool-or-num?
        (fn [x] (or (number? x) (boolean? x)))
        
        bool-or-num-to-bool
        (fn [x] (or (and (boolean? x) x)
                    (boolean (not (zero? x)))))
        ]

    (merge
     (copy-parameter :medium ::document/medium :type #{"hot-water" "saturated-steam"}
                     :convert keyword)
     (copy-parameter :flow-temperature ::document/flow-temperature)
     (copy-parameter :return-temperature ::document/return-temperature)
     (copy-parameter :ground-temperature ::document/ground-temperature)
     (copy-parameter :steam-pressure ::document/steam-pressure)
     (copy-parameter :steam-velocity ::document/steam-velocity)
     (copy-parameter :pumping-overhead ::document/pumping-overhead)
     (copy-parameter :pumping-cost-per-kwh ::document/pumping-cost-per-kwh)
     (copy-parameter :objective ::document/objective
                     :type #{"network" "system"}
                     :convert keyword)
     (copy-parameter :consider-alternative-systems
                     ::document/consider-alternatives
                     :type    bool-or-num?
                     :convert bool-or-num-to-bool
                     )
     (copy-parameter :consider-insulation
                     ::document/consider-insulation
                     :type    bool-or-num?
                     :convert bool-or-num-to-bool
                     )
     
     (copy-parameter :npv-term ::document/npv-term)
     (copy-parameter :npv-rate ::document/npv-rate
                     :type number? :convert #(/ % 100.0))
     (copy-parameter :loan-term ::document/loan-term)
     (copy-parameter :loan-rate ::document/loan-rate
                     :type number? :convert #(/ % 100.0))
     
     (copy-parameter :mip-gap ::document/mip-gap)
     (copy-parameter :param-gap ::document/param-gap)
     (copy-parameter :max-runtime ::document/maximum-runtime)

     (copy-parameter :max-supplies ::document/maximum-supply-sites
                     :type number?
                     :convert int)
     
     {::document/capital-costs
      (let [capital-cost-names (set/map-invert capital-cost-names)]
        (->> (for [{:keys [name annualize recur period rate]
                    :or {recur false annualize false period 0 rate 0}}
                   (:rows capital-costs)
                   :let [name (get capital-cost-names name)]
                   :when name]
               [name
                {:annualize (boolean annualize)
                 :recur (boolean recur)
                 :period (int (or period 0))
                 :rate (/ (or rate 0) 100)}])
             (into {})))}
     
     {::document/pumping-emissions
      (->>
       (for [e candidate/emissions-types
             :let [v (get parameters (common/to-keyword (str "pumping-" (name e))))]]
         [e (if (number? v) (/ v (candidate/emissions-factor-scales e)) 0)])
       (into {}))}
     
     {::document/emissions-cost
      (->>
       (for [e candidate/emissions-types
             :let [c (get parameters (common/to-keyword (str (name e) "-cost")))]]
         [e (if (number? c) c 0)])
       (into {}))}

     {::document/emissions-limit
      (->>
       (for [e candidate/emissions-types
             :let [c (get parameters (common/to-keyword (str (name e) "-limit")))]]
         [e (if (number? c) {:enabled true :value c} {:enabled false :value nil})])
       (into {}))
      }
     
     {::document/tariffs
      (-> (for [{:keys [tariff-name unit-rate capacity-charge standing-charge]}
                (:rows tariffs)]
            #::tariff
            {:name tariff-name
             :standing-charge standing-charge
             :unit-charge (/ unit-rate 100)
             :capacity-charge capacity-charge})
          (common/index ::tariff/id))
      
      ::document/connection-costs
      (-> (for [{:keys [cost-name fixed-cost capacity-cost]}
                (:rows connection-costs)]
            #::tariff
            {:name cost-name
             :fixed-connection-cost fixed-cost
             :variable-connection-cost capacity-cost})
          (common/index ::tariff/cc-id))
      
      ::document/alternatives
      (-> (for [{:keys [name fixed-cost capacity-cost operating-cost heat-price
                        co2 pm25 nox tank-factor]
                 }
                (:rows individual-systems)]
            #::supply
            {:name name
             :cost-per-kwh (/ heat-price 100.0)
             :capex-per-kwp capacity-cost
             :opex-per-kwp operating-cost
             :fixed-cost fixed-cost
             :kwp-per-mean-kw tank-factor
             :emissions
             {:co2  (/ (or co2 0.0) (candidate/emissions-factor-scales :co2))
              :pm25 (/ (or pm25 0.0) (candidate/emissions-factor-scales :pm25))
              :nox  (/ (or nox 0.0) (candidate/emissions-factor-scales :nox))}})
          (common/index ::supply/id))
      
      ::document/insulation
      (-> (for [{:keys [name fixed-cost cost-per-m2
                        maximum-reduction-% maximum-area-%
                        surface]}
                (:rows insulation)]
            #::measure
            {:name name
             :fixed-cost fixed-cost
             :cost-per-m2 cost-per-m2
             :maximum-effect (/ maximum-reduction-% 100.0)
             :maximum-area (/ maximum-area-% 100.0)
             :surface (keyword surface)})
          (common/index ::measure/id))
      
      ::document/pipe-costs
      (let [civils-keys (-> (:header pipe-costs)
                            (dissoc :nb :capacity :losses :pipe-cost))
            civils-ids   (common/index (set (vals civils-keys)))
            inverse      (set/map-invert civils-ids)

            civils-keys* (->> (for [[kwd s] civils-keys] [kwd (inverse s)])
                              (into {}))
            ]
        {:civils civils-ids
         :rows
         (->>
          (for [row (:rows pipe-costs)]
            [(:nb row)
             (let [basics
                   (cond-> {:pipe (:pipe-cost row 0)}
                     (:capacity row) (assoc :capacity-kw (:capacity row))
                     (:losses row)   (assoc :losses-kwh (:losses row)))]

               (reduce-kv
                (fn [a civ-key civ-id]
                  (let [cost (get row civ-key 0)]
                    (cond-> a
                      cost (assoc civ-id cost))))
                basics
                civils-keys*))])
          (into {}))})})))

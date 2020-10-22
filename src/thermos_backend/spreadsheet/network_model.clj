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
            [thermos-specs.measure :as measure]))

(def *100 (partial * 100.0))

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

(def candidate-columns
  [{ :name "ID"                  :key ::candidate/id}
   { :name "Category"            :key ::candidate/subtype}
   { :name "Address"             :key ::candidate/name}
   { :name "Constraint"  :key (comp name ::candidate/inclusion) }])

(defn building-columns [doc]
  (concat
   candidate-columns
   [{ :name "Annual demand (kWh)" :key ::demand/kwh              :format :double}
    { :name "Peak demand (kW)"    :key ::demand/kwp              :format :double}
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
    ]))

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



(defn output-network-parameters [ss doc]
  (-> ss
      (sheet/add-tab
       "Tariffs"
       [{:name "Tariff name" :key ::tariff/name}
        {:name "Unit rate (c/¤)" :key (comp *100 ::tariff/unit-charge)}
        {:name "Capacity charge (¤/kWp)" :key ::tariff/capacity-charge}
        {:name "Standing charge (¤)" :key ::tariff/standing-charge}]
       (vals (::document/tariffs doc)))
      
      (sheet/add-tab
       "Connection costs"
       [{:name "Cost name" :key ::tariff/name}
        {:name "Fixed cost (¤)" :key ::tariff/fixed-connection-cost}
        {:name "Capacity cost (¤/kWp)" :key ::tariff/variable-connection-cost}]
       (vals (::document/connection-costs doc)))

      (sheet/add-tab
       "Individual systems"
       (into
        [{:name "Name" :key ::supply/name}
         {:name "Fixed cost (¤)" :key ::supply/fixed-cost}
         {:name "Capacity cost (¤/kWp)" :key ::supply/capex-per-kwp}
         {:name "Operating cost (¤/kWp.yr)" :key ::supply/opex-per-kwp}
         {:name "Fuel price (c/kWh)" :key (comp *100 ::supply/cost-per-kwh)}
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
        {:name "Fixed cost" :key ::measure/fixed-cost}
        {:name "Cost / m2" :key ::measure/cost-per-m2}
        {:name "Maximum reduction %" :key (comp *100 ::measure/maximum-effect)}
        {:name "Maximum area %" :key (comp *100 ::measure/maximum-area)}
        {:name "Surface" :key (comp name ::measure/surface)}]
       (vals (::document/insulation doc)))
      
      
      (output-pipe-costs doc)
      
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

         ~@(for [[e f] (::pumping-emissions doc)]
             [(str "Pumping "
                   (candidate/text-emissions-labels e)
                   " ("
                   (candidate/emissions-factor-units e)
                   ")")
              (* (or f 0) (candidate/emissions-factor-scales e))])

         ~@(for [e candidate/emissions-types]
             [(str (candidate/text-emissions-labels e) " cost")
              (::document/emissions-cost e 0)])

         ~@(for [e candidate/emissions-types]
             [(str (candidate/text-emissions-labels e) " limit")
              (when (:enabled (::document/emissions-limit e))
                (:value (::document/emissions-limit e)))])
         
         
         ~["Objective" (name (::document/objective doc))]
         ~["Consider alternative systems" (::document/consider-alternatives doc)]
         ~["Consider insulation" (::document/consider-insulation doc)]
         
         ~["NPV Term" (::document/npv-term doc)]
         ~["NPV Rate" (*100 (::document/npv-rate doc))]

         ~["Loan Term" (::document/loan-term doc)]
         ~["Loan Rate" (*100 (::document/loan-rate doc))]
         
         ~["MIP Gap" (::document/mip-gap doc)]
         ~["Max runtime" (::document/maximum-runtime doc)]
         ]
       )
      
      )
  )

(defn output-paths [ss doc]
  (sheet/add-tab
   ss "Paths & pipes"
   (concat
    candidate-columns
    [{ :name "Length"              :key ::path/length}
     { :name "Civils"   :key #(document/civil-cost-name doc (::path/civil-cost-id %)) }]
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

(defn output-to-spreadsheet [ss doc]
  (-> ss
      (output-buildings doc)
      (output-paths doc)
      (output-network-parameters doc)
      ))

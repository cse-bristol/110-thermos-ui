(ns thermos-backend.spreadsheet.network-model
  "Functions to output network model data into a spreadsheet"
  (:require [thermos-backend.spreadsheet.common :as sheet]
            [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.tariff :as tariff]

            
            ))

(defn building-columns [doc]
  (into
   [{ :name "ID"                  :key ::candidate/id}
    { :name "Category"            :key ::candidate/subtype}
    { :name "Address"             :key ::candidate/name}
    { :name "Count"               :key ::demand/connection-count :format :int}
    { :name "Annual demand (kWh)" :key ::demand/kwh              :format :double}
    { :name "Peak demand (kW)"    :key ::demand/kwp              :format :double}
    { :name "Tariff name"         :key #(document/tariff-name doc (::tariff/id %))}
    { :name "Connection cost name" :key #(document/connection-cost-name doc (::tariff/cc-id %))}
    
    ]

   []
   ))




(defn output-buildings [ss doc]
  (sheet/add-tab
   ss "Buildings"
   (building-columns doc)
   (filter candidate/is-building? (vals (::document/candidates doc)))))

(defn output-paths [ss doc]
  ss)

(defn output-to-spreadsheet [ss doc]
  (-> ss
      (output-buildings doc)
      (output-paths doc)))

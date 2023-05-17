;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.tem
  (:require [dk.ative.docjure.spreadsheet :as x]
            [clojure.java.io :as io]
            [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.tariff :as tariff]
            [thermos-specs.solution :as solution]
            [thermos-specs.path :as path]
            [thermos-specs.supply :as supply])
  (:import
   [org.apache.poi.ss.util CellReference AreaReference]))

(defn add-table! [table table-name sheet]
  (let [table-id (inc (reduce max 1 (map #(.getId %) (.getTables sheet))))
        table-obj (.createTable
                   sheet
                   (-> (.getWorkbook sheet)
                       (.getCreationHelper)
                       (.createAreaReference
                        (CellReference. 0 0)
                        (CellReference. (dec (count table)) (dec (count (first table)))))))
        
        ]
    (.setName table-obj table-name)
    (.setDisplayName table-obj table-name)
    (.addNewTableStyleInfo (.getCTTable table-obj))
    (.setName (.getTableStyleInfo (.getCTTable table-obj)) "TableStyleMedium2")    
    (x/add-rows! sheet table)))

(comment
  (let [template      (x/load-workbook-from-resource "tem-template.xlsx")
        demands-sheet (x/select-sheet "Input Demands" template)]
    (add-table!
     [["Heading1" "Heading2"]
      ["Row 1 1"   "Row 1 2"]
      ["Row 2 1"   "Row 2 2"]
      ["Row 3 1"   "Row 3 2"]]
     "Test_table"
     demands-sheet)
    
    (x/save-workbook-into-file! "/home/hinton/test.xlsx" template)
    
    )
  )

(defn make-demands-table  [state]
  (let [mode (document/mode state)]
    `[["ID" "AnnualDemand" "PeakDemand"]
      ~@(for [c (vals (::document/candidates state))
              :when (and (candidate/is-building? c)
                         (candidate/is-connected? c))]
          [(::candidate/id c)
           (candidate/solved-annual-demand c mode)
           (candidate/solved-peak-demand c mode)

           ;; user defined fields (for toid)
           ])]))

(defn make-pipes-table    [state]
  `[["ID" "Diameter" "Losses" "kW" "Length" "Surface" "Diversity"]
    ~@(for [c (vals (::document/candidates state))
            :when (and (candidate/is-path? c)
                       (candidate/is-connected? c))]
        [(::candidate/id c)
         (::solution/diameter-mm c)
         (::solution/losses-kwh c)
         (::solution/capacity-kw c)
         (::path/length c)
         (document/civil-cost-name state (::path/civil-cost-id c))
         (::solution/diversity c)])])

(defn make-supplies-table [state]
  `[["ID" "AnnualOutput" "PeakOutput"]
    ~@(for [c (vals (::document/candidates state))
            :when (candidate/has-supply? c)]
        [(::candidate/id c)
         (::solution/output-kwh c)
         (::solution/capacity-kw c)])])

(defn write-tem-to-stream [out state]
  (let [template       (x/load-workbook-from-resource "tem-template.xlsx")
        demands-sheet  (x/select-sheet "Input Demands" template)
        pipes-sheet    (x/select-sheet "Input Pipes" template)
        supplies-sheet (x/select-sheet "Input Supplies" template)]
    (add-table! (make-demands-table state)  "demands"  demands-sheet)
    (add-table! (make-pipes-table state)    "pipes"    pipes-sheet)
    (add-table! (make-supplies-table state) "supplies" supplies-sheet)
    (x/save-workbook-into-stream! out template)))


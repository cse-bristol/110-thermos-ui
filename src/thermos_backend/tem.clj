;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.tem
  "Functions to export SEL's techno-economic model from a THERMOS problem."
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
   [org.apache.poi.ss.util CellReference AreaReference]
   [org.apache.poi.xssf.usermodel XSSFWorkbook XSSFRow]))

(defn insert-into-table! [data table-name ^XSSFWorkbook workbook]
  (let [table (.getTable workbook table-name)]
    (assert table (str "No table named " table-name " in workbook"))
    (let [area  (.getArea table)
          sheet (.getSheet workbook (.getSheetName table))
          left  (.getStartColIndex table)
          top   (.getStartRowIndex table)]
      (doseq [cell-ref (.getAllReferencedCells area)]
        (-> sheet
            (.getRow (.getRow cell-ref))
            (.getCell (int (.getCol cell-ref))
                      org.apache.poi.ss.usermodel.Row$MissingCellPolicy/CREATE_NULL_AS_BLANK)
            (.setBlank)))

      (.setArea table
                (-> (.getCreationHelper workbook)
                    (.createAreaReference
                     (CellReference. top left)
                     (CellReference. (dec (+ top (count data)))
                                     (dec (+ left (count (first data))))))))

      (doseq [[i row] (map-indexed vector data)]
        (let [row-obj (.getRow sheet i)]
          (doseq [[j col] (map-indexed vector row)]
            (let [cell (.getCell row-obj (int j)
                                 org.apache.poi.ss.usermodel.Row$MissingCellPolicy/CREATE_NULL_AS_BLANK)]
              (x/set-cell! cell col)
              )))))))

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

(defn populate-tem [state]
  (let [template       (x/load-workbook-from-resource "tem-template.xlsx")
        demands-sheet  (x/select-sheet "Input Demands" template)
        pipes-sheet    (x/select-sheet "Input PIpes" template)
        supplies-sheet (x/select-sheet "Input Supplies" template)]
    (insert-into-table! (make-demands-table state)  "demands"  demands-sheet)
    (insert-into-table! (make-pipes-table state)    "pipes"    pipes-sheet)
    (insert-into-table! (make-supplies-table state) "supplies" supplies-sheet)
    (.setForceFormulaRecalculation template true)
    template))

(defn write-tem-to-stream [out state]
  (->> (populate-tem state)
       (x/save-workbook-into-stream! out)))

(comment
  (write-tem-to-file "/home/hinton/tem.xlsx" -last-state)
  )

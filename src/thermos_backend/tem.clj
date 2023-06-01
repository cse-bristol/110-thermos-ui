;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.tem
  "Functions to export SEL's techno-economic model from a THERMOS problem."
  (:require [dk.ative.docjure.spreadsheet :as x]
            [clojure.java.io :as io]
            [clojure.string :as string]
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
    
    ;; (-> table (.getCTTable) (.addNewTableStyleInfo))
    ;; (-> table (.getCTTable) (.getTableStyleInfo) (.setName "TableStyleMedium2"))

    
    (let [area  (.getArea table)
          sheet (.getSheet workbook (.getSheetName table))
          left  (.getStartColIndex table)
          top   (.getStartRowIndex table)]
      (doseq [cell-ref (.getAllReferencedCells area)]
        (let [r (.getRow sheet (.getRow cell-ref))
              c (.getCell r (int (.getCol cell-ref))
                          org.apache.poi.ss.usermodel.Row$MissingCellPolicy/CREATE_NULL_AS_BLANK)]
          (.setBlank c)
          (.removeCell r c)))

      (.setArea table
                (-> (.getCreationHelper workbook)
                    (.createAreaReference
                     (CellReference. top left)
                     (CellReference. (dec (+ top (count data)))
                                     (dec (+ left (count (first data))))))))

      (doseq [[i row] (map-indexed vector data)]
        (let [row-obj (or (.getRow sheet (+ top i))
                          (.createRow sheet (+ top i)))]
          (doseq [[j col] (map-indexed vector row)]
            (let [cell (.getCell row-obj (+ (int j) left)
                                 org.apache.poi.ss.usermodel.Row$MissingCellPolicy/CREATE_NULL_AS_BLANK)]
              (x/set-cell! cell col)
              )))))))

(defn- add-user-fields [candidates standard-header standard-accessors]
  (let [user-fields
        (reduce
         (fn [fields candidate]
           (into fields (keys (::candidate/user-fields candidate))))
         #{} candidates)

        
        user-fields (let [standard-header (set (map string/lower-case standard-header))]
                      (vec (for [uf (sort user-fields)
                                 :when (not (contains? standard-header (string/lower-case uf)))] uf)))
        ]
    `[~(into (vec standard-header) user-fields)
      ~@(for [c candidates]
          (into (vec (for [a standard-accessors] (a c)))
                (let [candidate-fields (::candidate/user-fields c)]
                  (for [f user-fields] (get candidate-fields f)))))]))

(defn make-demands-table  [state]
  (let [mode (document/mode state)]
    (add-user-fields
     (filter
      #(and (candidate/is-building? %) (candidate/is-connected? %))
      (vals (::document/candidates state)))

     ["ID" "AnnualDemand" "PeakDemand" "ConnectionCount"]
     [::candidate/id
      #(candidate/solved-annual-demand % mode)
      #(candidate/solved-peak-demand % mode)
      ::demand/connection-count])))

(defn make-pipes-table    [state]
  (add-user-fields
   (filter
    #(and (candidate/is-path? %) (candidate/is-connected? %))
    (vals (::document/candidates state)))
   ["ID" "Diameter" "Losses" "kW" "Length" "Surface" "Diversity"]
   [::candidate/id
    ::solution/diameter-mm
    ::solution/losses-kwh
    ::solution/capacity-kw
    ::path/length
    #(document/civil-cost-name state (::path/civil-cost-id %))
    ::solution/diversity]))

(defn make-supplies-table [state]
  (add-user-fields
   (filter candidate/has-supply? (vals (::document/candidates state)))
   ["ID" "AnnualOutput" "PeakOutput"]
   [::candidate/id
    ::solution/output-kwh
    ::solution/capacity-kw]))

(defn populate-tem [state]
  (def -last-state state)
  (let [template       (x/load-workbook-from-resource "tem-template.xlsx")]
    (insert-into-table! (make-demands-table state)  "demands"  template)
    (insert-into-table! (make-pipes-table state)    "pipes"    template)
    (insert-into-table! (make-supplies-table state) "supplies" template)
    (.setForceFormulaRecalculation template true)
    template))

(defn write-tem-to-stream [out state]
  (->> (populate-tem state)
       (x/save-workbook-into-stream! out)))

(comment
  (->> (populate-tem -last-state)
       (x/save-workbook-into-file! "/home/hinton/tem.xlsx"))
  )

(ns thermos-backend.spreadsheet.common
  "Utility functions for spreadsheet output.

  Uses a simple internal representation of spreadsheets, assumed to
  have 1 table per page, before doing any writing.
  
  This does mean we can't write really really large spreadsheets.

  The representation is that a spreadsheet is a vector of tabs.

  Each tab is a map that contains:

  - :name, a string, the name of the tab
  - :columns, a seq of columns, each having :name :key and :format
  - :rows, a seq of things which will have columns' `:key`s applied to them
  "
  (:require [dk.ative.docjure.spreadsheet :as dj]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.logging :as log])
  (:import  [org.apache.poi.xssf.usermodel XSSFWorkbook]
            [org.apache.poi.ss.usermodel DataFormatter CellType Cell]))

(defn empty-spreadsheet
  "Make a new empty spreadsheet."
  [] [])

(let [fmt (DataFormatter.)]
  (defn cell-text [^Cell cell]
    (.formatCellValue fmt cell)))

(defn add-tab [ss name columns rows]
  (conj
   ss
   {:name    name
    :columns columns
    :rows    rows}))

(defn make-rows [{:keys [rows columns]}]
  (into
   [(mapv :name columns)]
   (map
    (fn [row] (mapv (fn [{:keys [key]}]
                      (try (key row)
                           (catch Exception e
                             (str "ERR" (.getMessage e))))) columns))
    rows)))

(defn style-header-row [sheet header-style]
  (dj/set-row-style! (first (dj/row-seq sheet)) header-style)
  sheet)

(defn write-to-stream [ss out]
  (let [wb (XSSFWorkbook.)
        header-style (dj/create-cell-style! wb {:font {:bold true}})
        
        wb (reduce
            (fn [wb tab]
              (let [sheet (dj/add-sheet! wb (:name tab))]
                (dj/add-rows! sheet (make-rows tab))
                (style-header-row sheet header-style)
                wb))
            wb
            ss)]
    (dj/save-workbook-into-stream! out wb)))

(defmethod dj/load-workbook java.io.File
  [^java.io.File f] (dj/load-workbook (.getCanonicalPath f)))

(defn strip-units [s] (and s (string? s) (string/replace s #" *\(.+$" "")))

(defn to-keyword [s]
  (let [s (str s)
        s (string/lower-case s)
        s (strip-units s)
        s (and s (string/replace s "/" "-per-"))
        s (and s (string/replace s #"[- _:]+" "-"))
        s (and s (string/replace s "₂" "2"))
        s (and s (string/replace s "₅" "5"))
        s (and s (string/replace s "ₓ" "x"))
        ]

    (keyword s)))

(defn index 
  "Given a collection of entries, assign an index to each one and
   create a map keyed from index to entry. 
   
   If `id-key` is specified, also add the index as a field to each entry."
  [entries & [id-key]]
  (into {}
        (map-indexed
         (fn [i v] [i (cond-> v id-key (assoc id-key i))])
         entries)))

(defn top-left-rectangle
  "Read a table out of a sheet.

  The tables columns are determined by the first row, starting at A1.

  Headers are read until an empty column.
  Subsequent rows are read until a row with no values in the header.

  Junk outside the rectangle is ignored.

  Headers are converted to keywords.
  Rows are returned as maps against these keywords
  "
  [sheet]

  (when sheet
    (let [[header & rows]
          (remove nil? (dj/row-seq sheet))

          header-cells (dj/cell-seq header)
          header-cells (take-while
                        #(not
                          (or
                           (nil? %)
                           (= CellType/BLANK (.getCellType ^Cell %))))
                        header-cells)
          header-text (map (comp strip-units cell-text) header-cells)
          header-keys (map to-keyword header-text)
          n-cols      (count header-keys)
          ]
      
      {:header (zipmap header-keys header-text)
       :rows
       (map-indexed
        (fn [i r]
          (-> (zipmap header-keys r)
              (assoc :spreadsheet/row i)))
        (->> rows
             (map (fn [row]
                    (let [cells (dj/cell-seq row)
                          vals  (map dj/read-cell (take n-cols cells))
                          ]

                      vals
                      )
                    )
)
             (take-while #(some identity %))))})))

(defn read-to-tables [file]
  (let [wb (dj/load-workbook file)]
    (into
     {}
     (for [sheet (dj/sheet-seq wb)]
       [(to-keyword (dj/sheet-name sheet))
        (top-left-rectangle sheet)]))))

(comment
  (read-to-tables
   "/home/hinton/dl/cddp-thermos-parameters-2021-01-25.xlsx"
   )
  )

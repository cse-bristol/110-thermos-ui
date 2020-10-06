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
            [clojure.java.io :as io])
  (:import  [org.apache.poi.xssf.usermodel XSSFWorkbook]))

(defn empty-spreadsheet
  "Make a new empty spreadsheet."
  [] [])

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
    (fn [row] (mapv (fn [{:keys [key]}] (try (key row)
                                             (catch Exception e
                                               (str "ERR" (.getMessage e))))) columns))
    rows)))

(defn style-header-row [sheet header-style]
  (dj/set-row-style! (first (dj/row-seq sheet)) header-style)
  sheet)

(defn write-to-stream [ss out]
  (let [wb (XSSFWorkbook.)
        header-style (dj/create-cell-style! wb {:font {:bold true}})
        
        wb (try (reduce
                 (fn [wb tab]
                   (let [sheet (dj/add-sheet! wb (:name tab))]
                     (dj/add-rows! sheet (make-rows tab))
                     (style-header-row sheet header-style)
                     wb))
                 wb
                 ss)
                (catch Exception e
                  (.printStackTrace e)
                  )
                )]
    (dj/save-workbook-into-stream! out wb)))

(comment

  (with-open [o (io/output-stream (io/file "/home/hinton/tmp/out.xlsx"))]
    (-> (empty-spreadsheet)
        (add-tab "Stuff"
                 [{:name "Column 1" :key :a}
                  {:name "Column 2" :key :b}]

                 [{:a 1 :b 2}
                  {:a 3 :b 4}]
                 )
        (write-to-stream o)
        )
    )
  )

(ns thermos-backend.spreadsheet.core
  "Main entrypoint for making spreadsheets from documents and vice-versa"
  (:require [thermos-backend.spreadsheet.common :as common]
            [thermos-backend.spreadsheet.network-model :as network-model]
            [thermos-backend.spreadsheet.supply-model :as supply-model]))

(defn to-spreadsheet [doc]
  (-> (common/empty-spreadsheet)
      (network-model/output-to-spreadsheet doc)
      (supply-model/output-to-spreadsheet doc)))

(defn from-spreadsheet [file]
  (-> (common/read-to-tables file)
      (network-model/input-from-spreadsheet)))

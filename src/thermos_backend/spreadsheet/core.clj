(ns thermos-backend.spreadsheet.core
  "Main entrypoint for making spreadsheets from documents"
  (:require [thermos-backend.spreadsheet.common :as common]
            [thermos-backend.spreadsheet.network-model :as network-model]))

(defn to-spreadsheet [doc]
  (-> (common/empty-spreadsheet)
      (network-model/output-to-spreadsheet doc))
  )


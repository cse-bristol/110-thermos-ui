;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.spreadsheet.core
  "Main entrypoint for making spreadsheets from documents and vice-versa"
  (:require [thermos-backend.spreadsheet.common :as common]
            [thermos-backend.spreadsheet.network-model :as network-model]
            [thermos-backend.spreadsheet.supply-model :as supply-model]
            [thermos-backend.spreadsheet.schema :as schema]))

(defn to-spreadsheet [doc]
  (-> (common/empty-spreadsheet)
      (network-model/output-to-spreadsheet doc)
      (supply-model/output-to-spreadsheet doc)))

(defn from-spreadsheet [file]
  (let [{errors :import/errors :as spreadsheet}
        (schema/validate-spreadsheet (common/read-to-tables file))]

    (if (nil? errors)
      (merge (network-model/input-from-spreadsheet spreadsheet)
             (supply-model/input-from-spreadsheet spreadsheet))

      {:import/errors errors})))

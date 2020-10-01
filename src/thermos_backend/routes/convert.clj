(ns thermos-backend.routes.convert
  (:require [thermos-backend.spreadsheet.core :as xl]
            [thermos-backend.spreadsheet.common :as xlc]
            [ring.util.response :as response]
            [ring.util.io :as rio]
            [thermos-backend.pages.cache-control :as cache-control]
            [thermos-util.converter :as geojson-converter]
            [cheshire.core :as json]
            ))

(def xl-content-type "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")

(defn problem-to-excel [{{state :state} :body-params}]
  (-> (rio/piped-input-stream
       (fn [out]
         (-> (xl/to-spreadsheet state)
             (xlc/write-to-stream out))
         (.flush out)
         ))
      (response/response)
      (response/content-type xl-content-type)
      (cache-control/no-store)))

(defn problem-to-json [{{state :state} :body-params}]
  (-> (geojson-converter/network-problem->geojson state)
      (json/encode)
      (response/response)
      (response/content-type "application/json")
      (cache-control/no-store)))

(def converter-routes
  ["/convert"
   [["/excel" problem-to-excel]
    ["/json" problem-to-json]]])


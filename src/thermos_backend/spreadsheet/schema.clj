(ns thermos-backend.spreadsheet.schema
    (:require [malli.provider :as mp]
              [malli.core :as m]
              [malli.error :as me]
              [malli.transform :as mt]))

(defn number-to-double-transformer []
  (mt/transformer
   {:name :double
    :decoders {'double? mt/-number->double, :double mt/-number->double}}))
  
(def network-model-schema 
  [:map
   [:tariffs [:map
              [:header [:map
                        [:tariff-name string?]
                        [:unit-rate string?]
                        [:capacity-charge string?]
                        [:standing-charge string?]]]
              [:rows [:sequential [:map
                                   [:tariff-name string?]
                                   [:unit-rate double?]
                                   [:capacity-charge double?]
                                   [:standing-charge double?]
                                   [:spreadsheet/row int?]]]]]]
   
   [:connection-costs [:map
                       [:header [:map
                                 [:cost-name string?]
                                 [:fixed-cost string?]
                                 [:capacity-cost string?]]]
                       [:rows [:sequential [:map
                                            [:cost-name string?]
                                            [:fixed-cost double?]
                                            [:capacity-cost double?]]]]]]
   
   [:individual-systems [:map
                         [:header [:map
                                   [:name string?]
                                   [:fixed-cost string?]
                                   [:capacity-cost string?]
                                   [:operating-cost string?]
                                   [:fuel-price string?]
                                   [:co2 string?]
                                   [:pm25 string?]
                                   [:nox string?]]]
                         [:rows [:sequential [:map
                                              [:nox double?]
                                              [:spreadsheet/row int?]
                                              [:fixed-cost double?]
                                              [:name string?]
                                              [:pm25 double?]
                                              [:capacity-cost empty?]
                                              [:co2 double?]
                                              [:operating-cost double?]
                                              [:fuel-price double?]]]]]]
   
   [:pipe-costs [:map
                 [:header [:map
                           [:nb string?]
                           [:capacity string?]
                           [:losses string?]
                           [:pipe-cost string?]
                           [:soft string?]
                           [:hard string?]]]
                 [:rows [:sequential [:map
                                      [:nb double?]
                                      [:capacity double?]
                                      [:losses double?]
                                      [:pipe-cost double?]
                                      [:soft double?]
                                      [:hard double?]
                                      [:spreadsheet/row int?]]]]]]
   
   [:insulation [:map
                 [:header [:map
                           [:name string?]
                           [:fixed-cost string?]
                           [:cost-per-m2 string?]
                           [:maximum-reduction-% string?]
                           [:maximum-area-% string?]
                           [:surface string?]]]
                 [:rows [:sequential [:map
                                      [:name string?]
                                      [:fixed-cost double?]
                                      [:cost-per-m2 double?]
                                      [:maximum-reduction-% double?]
                                      [:maximum-area-% double?]
                                      [:surface double?]
                                      [:spreadsheet/row int?]]]]]]
   
   [:other-parameters [:map
                       [:header [:map
                                 [:parameter string?]
                                 [:value string?]]]
                       [:rows [:sequential [:map
                                            [:parameter string?]
                                            [:value any?]
                                            [:spreadsheet/row int?]]]]]]])


(defn validate-network-model-ss 
  "Coerce and validate a network model spreadsheet.
   
   Returns a map with 2 elements, :errors and :spreadsheet :errors is either a map
   containg errors or nil, and :spreadsheet is the coerced input. 
   
   Current attempted coercions are string-to-number and int-to-double."
  [ss]
  (let [coercions (mt/transformer mt/string-transformer number-to-double-transformer)
        coerced (m/decode network-model-schema ss coercions)]
    { :errors (me/humanize (m/explain network-model-schema coerced))
      :spreadsheet coerced}))


;; Write default spreadsheet to disk
(comment
  (require '[thermos-specs.defaults :as defaults])
  (require '[thermos-backend.spreadsheet.core :as ss-core])
  (require '[thermos-backend.spreadsheet.common :as ss-common])
  (require '[clojure.java.io :as io])
  
  (let [out-sheet (ss-core/to-spreadsheet defaults/default-document)]
    (with-open [out (io/output-stream "/home/neil/tmp/spreadsheet.xlsx")]
      (ss-common/write-to-stream out-sheet out))))

;; Generate bits of schema from default spreadsheet
;; The generated forms need some adjusting but they're a starting point
(comment
  (require '[malli.provider :as mp])
  (require '[thermos-backend.spreadsheet.common :as ss-common])

  (let [in-ss (ss-common/read-to-tables "/home/neil/tmp/spreadsheet.xlsx")
        sheet (:connection-costs in-ss)]
    (println sheet)
    (mp/provide sheet)))

;; Test schema
(comment
  (require '[thermos-backend.spreadsheet.common :as ss-common])

  (let [in-ss (ss-common/read-to-tables "/home/neil/tmp/spreadsheet.xlsx")]
    (validate-network-model-ss in-ss)))

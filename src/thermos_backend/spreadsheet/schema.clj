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
                           [:capacity {:optional true} string?]
                           [:losses {:optional true} string?]
                           [:pipe-cost string?]]]
                 [:rows [:sequential [:map
                                      [:nb number?]
                                      [:capacity {:optional true} number?]
                                      [:losses {:optional true} number?]
                                      [:pipe-cost number?]
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


(def variable-pipe-costs-schema [:map 
                                 [:header [:map-of keyword? string?]] 
                                 [:rows [:sequential [:map-of keyword? number?]]]])

(defn merge-errors
  "Recursively merges error maps."
  [& maps]
  (letfn [(do-merge [& xs]
            (cond 
              (every? map? xs) (apply merge-with do-merge xs)
              (every? sequential? xs) (apply mapv do-merge xs)
              :else (last (remove nil? xs))))]
    (reduce do-merge maps)))

(defn validate-pipe-costs 
  "Coerce and validate the non-fixed columns in the pipe costs sheet.
   Unfortunately malli is not (currently) capable of doing this
   at the same time as validating the fixed columns - see https://github.com/metosin/malli/issues/43"
  [pipe-costs coercions]
  (let [coerced (m/decode variable-pipe-costs-schema pipe-costs coercions)]
    (assoc pipe-costs :import/errors (me/humanize (m/explain variable-pipe-costs-schema coerced)))))

(defn validate-network-model-ss 
  "Coerce and validate a network model spreadsheet.
   
   Returns the coerced input with an extra key, :import/errors,
   which is either nil or an error map.
   
   Current attempted coercions are string-to-number and int-to-double."
  [ss]
  (let [coercions (mt/transformer mt/string-transformer number-to-double-transformer)

        coerced (m/decode network-model-schema ss coercions)
        
        coerced-pcs  (m/decode variable-pipe-costs-schema (:pipe-costs coerced) coercions)

        coerced (conj coerced (when coerced-pcs [:pipe-costs coerced-pcs]))
        
        errors (me/humanize (m/explain network-model-schema coerced))

        pc-errors  (me/humanize (m/explain variable-pipe-costs-schema (:pipe-costs coerced)))]
    
    (assoc coerced :import/errors (merge-errors (when pc-errors {:pipe-costs pc-errors}) errors))))


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
        sheet (:pipe-costs in-ss)]
    (println sheet)
    (mp/provide sheet)))

;; Test schema
(comment
  (require '[thermos-backend.spreadsheet.common :as ss-common])

  (let [in-ss (ss-common/read-to-tables "/home/neil/tmp/spreadsheet.xlsx")]
    (validate-network-model-ss in-ss)))

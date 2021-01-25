(ns thermos-backend.spreadsheet.schema
  (:require [malli.provider :as mp]
            [malli.core :as m]
            [malli.error :as me]
            [malli.transform :as mt]
            [com.rpl.specter :as S]))

(defn- number-to-double-transformer []
  (mt/transformer
   {:name :double
    :decoders {'double? mt/-number->double, :double mt/-number->double}}))

(def network-model-schema 
  [:map
   {:error/message "missing sheet from spreadsheet"}
   [:tariffs 
    [:map
     [:header [:map
               {:error/message "column missing"}
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
   
   [:connection-costs
    [:map
     [:header [:map
               {:error/message "column missing"}
               [:cost-name string?]
               [:fixed-cost string?]
               [:capacity-cost string?]]]
     [:rows [:sequential [:map
                          [:cost-name string?]
                          [:fixed-cost double?]
                          [:capacity-cost double?]]]]]]

   [:individual-systems
    [:map
     [:header [:map
               {:error/message "column missing"}
               [:name string?]
               [:fixed-cost string?]
               [:capacity-cost string?]
               [:operating-cost string?]
               [:heat-price string?]
               [:co2 string?]
               [:pm25 string?]
               [:nox string?]]]
     [:rows [:sequential
             [:map
              [:spreadsheet/row int?]

              [:fixed-cost double?]
              [:name string?]
              [:capacity-cost double?]
              [:nox [:or nil? double?]]
              [:pm25 [:or nil? double?]]
              [:co2 [:or nil? double?]]
              [:operating-cost double?]
              [:heat-price double?]]]]]]

   [:pipe-costs
    [:map
     [:header [:map
               {:error/message "column missing"}
               [:nb string?]
               [:capacity {:optional true} string?]
               [:losses {:optional true} string?]
               [:pipe-cost string?]]]
     [:rows [:sequential [:map
                          [:nb number?]
                          [:capacity {:optional true} [:or number? nil?]]
                          [:losses {:optional true} [:or number? nil?]]
                          [:pipe-cost number?]
                          [:spreadsheet/row int?]]]]]]

   [:insulation 
    [:map
     [:header [:map
               {:error/message "column missing"}
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
                          [:surface string?]
                          [:spreadsheet/row int?]]]]]]

   [:other-parameters 
    [:map
     [:header [:map
               {:error/message "column missing"}
               [:parameter string?]
               [:value string?]]]
     [:rows [:sequential [:map
                          [:parameter string?]
                          [:value any?]
                          [:spreadsheet/row int?]]]]]]])


(def variable-pipe-costs-schema
  [:map
   {:error/message "missing sheet from spreadsheet"}
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

(defn validate-network-model-ss 
  "Coerce and validate a network model spreadsheet.
   
   Returns the coerced input with an extra key, :import/errors,
   which is either nil or an error map.
   
   Current attempted coercions are string-to-number and int-to-double."
  [ss]
  (let [coercions (mt/transformer mt/string-transformer number-to-double-transformer)

        coerced (m/decode network-model-schema ss coercions)
        ;; the variable pipe costs schema cannot properly check rows
        ;; because I (tom) designed that datastructure badly: it
        ;; contains a few special columns which are in the normal
        ;; schema, that have to be removed before checking with
        ;; variable schema

        variable-pipe-costs (->> (:pipe-costs coerced)
                                 (S/setval [(S/multi-path :headers [:rows S/ALL]) 
                                            (S/multi-path :losses :capacity)]
                                           S/NONE))
        
        coerced-variable-pcs  (m/decode variable-pipe-costs-schema variable-pipe-costs coercions)

        
        coerced (cond-> coerced
                  (and coerced-variable-pcs (contains? coerced :pipe-costs))
                  (update-in [:pipe-costs :rows]
                             (fn [rows]
                               (map merge rows (:rows coerced-variable-pcs)))))


        errors (me/humanize (m/explain network-model-schema coerced))

        pc-errors  (me/humanize (m/explain variable-pipe-costs-schema coerced-variable-pcs))
        ]
    
    
    (assoc coerced :import/errors (merge-errors (when pc-errors {:pipe-costs pc-errors}) errors))))



;; Write default spreadsheet to disk
(comment
  (require '[thermos-specs.defaults :as defaults])
  (require '[thermos-backend.spreadsheet.core :as ss-core])
  (require '[thermos-backend.spreadsheet.common :as ss-common])
  (require '[clojure.java.io :as io])
  
  (let [default-doc
        (merge defaults/default-document
               #:thermos-specs.document
                {:connection-costs
                 {0 #:thermos-specs.tariff
                     {:id 0
                      :name "cost-name"
                      :fixed-connection-cost 123
                      :variable-connection-cost 456}}
                 :insulation
                 {0 #:thermos-specs.measure
                     {:id 0
                      :name "name"
                      :fixed-cost 1
                      :cost-per-m2 2
                      :maximum-effect 0.1
                      :maximum-area 0.2
                      :surface :roof}}
                 :pumping-emissions {:co2 0.1 :pm25 0.2 :nox 0.3}
                 :emissions-cost {:co2 0.4 :pm25 0.5 :nox 0.6}
                 :emissions-limit {:co2 {:value 0.9 :enabled true}
                                   :pm25 {:value 0.8 :enabled true}
                                   :nox {:value 0.7 :enabled true}}})
        out-sheet (ss-core/to-spreadsheet default-doc)]
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

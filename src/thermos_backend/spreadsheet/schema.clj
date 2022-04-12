;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.spreadsheet.schema
  (:require [malli.provider :as mp]
            [malli.core :as m]
            [malli.error :as me]
            [malli.transform :as mt]
            [malli.util :as mu]
            [clojure.string :as string]
            [com.rpl.specter :as S]
            [thermos-backend.spreadsheet.common :as sheet]))

(def required-parameters
  #{:medium
    :flow-temperature
    :return-temperature
    :ground-temperature
    :pumping-overhead
    :pumping-cost-per-kwh
    :objective
    :consider-alternative-systems
    :consider-insulation
    :npv-term
    :npv-rate
    :loan-term
    :loan-rate
    :mip-gap
    :max-runtime
    :max-supplies
    })

(def required-supply-parameters
  #{:accounting-period
    :discount-rate
    :curtailment-cost
    :can-dump-heat
    :mip-gap
    :time-limit
    :default-profile})

(defn- number-to-double-transformer []
  (mt/transformer
   {:name :double
    :decoders {'double? mt/-number->double, :double mt/-number->double}}))

(defn number-to-boolean [n]
  (cond
    (= 1.0 n) true
    (= 0.0 n) false
    (= 1 n) true
    (= 0 n) false
    (nil? n) false
    :else n))

(defn- number-to-boolean-transformer []
  (mt/transformer
   {:name :boolean
    :decoders {'boolean? number-to-boolean, :boolean number-to-boolean}}))


(defn- schematise-dynamic-cols
  "Modify the schema so that any unknown columns in the given sheet
   have their cells validated and coerced."
  [schema ss sheet validator-fn]
  (if-let [header (get-in ss [sheet :header])]
    (let [cols (keys header)
          path [0 sheet :rows :sequential]
          sheet-schema (mu/merge
                        (reduce (fn [s col] (mu/assoc s col validator-fn)) [:map] cols)
                        (mu/get-in schema path))]
      (mu/assoc-in schema path sheet-schema))
    schema))

(defn- no-error?
  "Wrap a function in the format expected by malli's `:error/fn`, 
   and turn it into a function suitable for  malli's `:fn`, 
   which returns false if `error-fn`'s return value is non-nil, otherwise true."
  [error-fn]
  (fn [arg] (nil? (error-fn {:value arg} nil))))

(defn- duplicates 
  "Get the duplicate items in a collection"
  [coll]
  (->> coll
       (frequencies)
       (filter (fn [[_ freq]] (> freq 1)))
       (map first)))

(defn- unique?
  "Return a function that returns an error message if the entries in
   the given field are not unique."
  [fieldname]
  {:pre [(keyword? fieldname)]}
  (fn [{:keys [value]} _]
    (let [values (for [entry (:rows value)] (fieldname entry))
          dups (duplicates values)]
      (when (seq dups)
        (str "duplicate identifier: '" (string/join "', '" dups) "'")))))

(defn- keyword->string [k] (string/capitalize (string/replace (name k) \- \ )))

(defn- has-required-values? [fieldname required]
  {:pre [(keyword? fieldname)]}
  (fn [{:keys [value]} _]
    (let [present (set (for [entry (:rows value)] (sheet/to-keyword (fieldname entry))))
          missing (map keyword->string (apply (partial disj required) present))]
      (when (seq missing)
        (str "missing required value: '" (string/join "', '" missing) "'")))))

(defn- references?
  "Check the spreadsheet for referential integrity.
   
   Returns a function that returns an error message if any values in 
   `ref-path` are not also present in `source-path`, except nil.
   
   Values may be present in `source-path` that are not in `ref-path`.`"
  [source-path ref-path]
  (fn [{ss :value} _]
    (let [source-sheet (keyword->string (get source-path 0))
          ref-vals (filter some? (S/select ref-path ss))
          source-vals (set (S/select source-path ss))

          all-not-ok
          (for [ref-val ref-vals
                :when (not (contains? source-vals ref-val))] ref-val)]

      (case (count (set all-not-ok))
        0 nil
        1 (str "value '" (first all-not-ok) "' referenced, "
               "but not defined in sheet '" source-sheet "'")
        (str "values '" (string/join "', '" (set all-not-ok)) "' referenced, "
             "but not defined in sheet '" source-sheet "'")))))

(defn- profile-substation-names-match?
  "Return an error message if the names of substation profile columns do not
   match those defined in supply-substations.
   
  Allows the load-kw profile column to be missing."
  [a1 a2]
  ((references?
    [:supply-substations :rows S/ALL :name]
    [:supply-profiles :header S/MAP-VALS (S/regex-nav #"^Substation: (.+)$") (S/nthpath 1)]) a1 a2))

(defn- fuel-names-match?
  "Return an error message if the names of fuel profile columns do not match 
   the fuels referenced in supply-plant.
   
   Allows the CO₂, NOₓ and PM₂₅ columns to be missing."
  [a1 a2]
  ((references?
    [:supply-profiles :header S/MAP-VALS (S/regex-nav #"^Fuel: (.+) price$") (S/nthpath 1)]
    [:supply-plant :rows S/ALL :fuel]) a1 a2))

(defn- default-heat-profile-matches?
  "Return an error message if the default heat profile does
   not match any of the heat profile columns in supply-profiles."
  [a1 a2]
  ((references?
    [:supply-profiles :header S/MAP-VALS (S/regex-nav #"^Profile: (.+)$") (S/nthpath 1)]
    [:supply-parameters :rows S/ALL #(= (:parameter %) "Default profile") :value]) a1 a2))

(defn- rows-per-day-type
  "Return an error message if the division counts in supply-day-types do not 
   match the number of rows per day type in supply-profiles."
  [{{:keys [supply-profiles supply-day-types]} :value} _]
  (let [profiles
        (->> (:rows supply-profiles)
             (group-by :day-type))

        all-not-ok
        (for [day-type (:rows supply-day-types)
              :let [divisions (:divisions day-type)
                    num-profile-rows (count (get profiles (:name day-type)))]
              :when (not= (int divisions) num-profile-rows)]
          (str "day type '" (:name day-type) "' has "
               (int divisions) " divisions, "
               "but had " num-profile-rows " entries in sheet 'supply profiles'"))]

    (when (seq all-not-ok)
      (string/join ". " all-not-ok))))

(defn- intervals-per-day-type
  "Return an error message if the division counts in supply-day-types do not 
   match the number of rows per day type in supply-profiles."
  [{{:keys [supply-profiles supply-day-types]} :value} _]
  (let [profiles
        (->> (:rows supply-profiles)
             (group-by :day-type))

        all-not-ok
        (for [day-type (:rows supply-day-types)
              :let [divisions (:divisions day-type)
                    profile-rows (map (fn [row] (double (:interval row))) 
                                      (get profiles (:name day-type)))]
              :when (not= (range 0.0 divisions) profile-rows)]
          (str "day type '" (:name day-type) "' has incorrectly numbered "
               "division intervals in sheet 'supply profiles'"))]

    (when (seq all-not-ok)
      (string/join ". " all-not-ok))))

(def spreadsheet-schema
  [:and
   [:map
    {:error/message "missing sheet from spreadsheet"}
    ;; Network model sheets
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

    [:capital-costs
     [:map
      [:header [:map {:error/message "column missing"}
                [:name string?]
                [:annualize string?]
                [:recur string?]
                [:period string?]
                [:rate string?]]]
      [:rows [:sequential [:map
                           [:name [:enum
                                   "Other heating"
                                   "Connections"
                                   "Insulation"
                                   "Supply"
                                   "Pipework"]]
                           [:annualize boolean?]
                           [:recur boolean?]
                           [:period {:optional true} [:or number? nil?]]
                           [:rate {:optional true} [:or double? nil?]]]]]]]

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
                [:nox string?]
                [:tank-factor string?]]]
      [:rows [:sequential
              [:map
               [:spreadsheet/row int?]
               [:fixed-cost double?]
               [:name string?]
               [:capacity-cost double?]
               [:nox {:optional true} [:or double? nil?]]
               [:pm25 {:optional true} [:or double? nil?]]
               [:co2 {:optional true} [:or double? nil?]]
               [:operating-cost double?]
               [:tank-factor {:optional true} [:or double? nil?]]
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
     [:and
      [:map
       [:header [:map
                 {:error/message "column missing"}
                 [:parameter string?]
                 [:value string?]]]
       [:rows [:sequential
               [:multi {:dispatch (comp sheet/to-keyword :parameter) :error/message "unknown parameter"}
                [:medium [:map [:value [:enum "hot-water" "saturated-steam"]]]]
                [:flow-temperature [:map [:value number?]]]
                [:return-temperature [:map [:value number?]]]
                [:ground-temperature [:map [:value number?]]]
                [:pumping-overhead [:map [:value number?]]]
                [:pumping-cost-per-kwh [:map [:value number?]]]
                [:pumping-co2 [:map [:value {:optional true} [:or number? nil?]]]]
                [:pumping-pm25 [:map [:value {:optional true} [:or number? nil?]]]]
                [:pumping-nox [:map [:value {:optional true} [:or number? nil?]]]]
                [:co2-cost [:map [:value {:optional true} [:or number? nil?]]]]
                [:pm25-cost [:map [:value {:optional true} [:or number? nil?]]]]
                [:nox-cost [:map [:value {:optional true} [:or number? nil?]]]]
                [:co2-limit [:map [:value {:optional true} [:or number? nil? [:enum "none"]]]]]
                [:pm25-limit [:map [:value {:optional true} [:or number? nil? [:enum "none"]]]]]
                [:nox-limit [:map [:value {:optional true} [:or number? nil? [:enum "none"]]]]]
                [:objective [:map [:value string?]]]
                [:consider-alternative-systems [:map [:value boolean?]]]
                [:consider-insulation [:map [:value boolean?]]]
                [:default-civil-cost [:map [:value {:optional true} [:or string? nil? [:enum "none"]]]]]
                [:npv-term [:map [:value number?]]]
                [:npv-rate [:map [:value number?]]]
                [:loan-term [:map [:value number?]]]
                [:loan-rate [:map [:value number?]]]
                [:mip-gap [:map [:value number?]]]
                [:param-gap [:map [:value {:optional true} [:or number? nil?]]]]
                [:max-runtime [:map [:value number?]]]
                [:max-supplies [:map [:value [:or number? [:enum "unlimited"]]]]]]]]]
      [:fn
       {:error/fn (has-required-values? :parameter required-parameters)
        :error/path [:header :parameter]}
       (no-error? (has-required-values? :parameter required-parameters))]]]

     ;; Supply model sheets
    [:supply-plant
     [:map
      [:header [:map
                {:error/message "column missing"}
                [:fixed-capex string?]
                [:capex-per-kwp string?]
                [:capex-per-kwh string?]
                [:name string?]
                [:capacity string?]
                [:fuel string?]
                [:heat-efficiency string?]
                [:power-efficiency string?]
                [:substation string?]
                [:fixed-opex string?]
                [:opex-per-kwp string?]
                [:opex-per-kwh string?]
                [:chp? string?]
                [:lifetime string?]]]
      [:rows [:sequential [:map
                           [:fixed-capex number?]
                           [:capex-per-kwp number?]
                           [:capex-per-kwh number?]
                           [:name string?]
                           [:capacity number?]
                           [:fuel string?]
                           [:heat-efficiency double?]
                           [:power-efficiency {:optional true} [:or double? nil?]]
                           [:substation {:optional true} [:or string? nil?]]
                           [:fixed-opex number?]
                           [:opex-per-kwp number?]
                           [:opex-per-kwh number?]
                           [:chp? boolean?]
                           [:lifetime number?]
                           [:spreadsheet/row int?]]]]]]

    [:supply-day-types
     [:and
      [:map
       [:header [:map
                 {:error/message "column missing"}
                 [:name string?]
                 [:divisions string?]
                 [:frequency string?]]]
       [:rows [:sequential [:map
                            [:name string?]
                            [:divisions number?]
                            [:frequency number?]
                            [:spreadsheet/row int?]]]]]
      [:fn
       {:error/fn (unique? :name)
        :error/path [:header :name]}
       (no-error? (unique? :name))]]]

    [:supply-storage
     [:map
      [:header [:map
                {:error/message "column missing"}
                [:name string?]
                [:lifetime string?]
                [:capacity-kwh string?]
                [:capacity-kwp string?]
                [:efficiency string?]
                [:fixed-capex string?]
                [:capex-per-kwp string?]
                [:capex-per-kwh string?]]]
      [:rows [:sequential [:map
                           [:name string?]
                           [:lifetime number?]
                           [:capacity-kwh number?]
                           [:capacity-kwp number?]
                           [:efficiency double?]
                           [:fixed-capex number?]
                           [:capex-per-kwp number?]
                           [:capex-per-kwh number?]
                           [:spreadsheet/row int?]]]]]]

    [:supply-substations
     [:and
      [:map
       [:header [:map
                 {:error/message "column missing"}
                 [:name string?]
                 [:headroom-kwp string?]
                 [:alpha string?]]]
       [:rows [:sequential [:map
                            [:name string?]
                            [:headroom-kwp number?]
                            [:alpha double?]
                            [:spreadsheet/row int?]]]]]
      [:fn
       {:error/fn (unique? :name)
        :error/path [:header :name]}
       (no-error? (unique? :name))]]]

    [:supply-parameters
     [:and
      [:map
       [:header [:map
                 {:error/message "column missing"}
                 [:parameter string?]
                 [:value string?]]]
       [:rows [:sequential
               [:multi {:dispatch (comp sheet/to-keyword :parameter) :error/message "unknown parameter"}
                [:accounting-period [:map [:value number?]]]
                [:discount-rate [:map [:value double?]]]
                [:curtailment-cost [:map [:value number?]]]
                [:can-dump-heat [:map [:value boolean?]]]
                [:mip-gap [:map [:value double?]]]
                [:time-limit [:map [:value number?]]]
                [:default-profile [:map [:value string?]]]]]]]
      [:fn
       {:error/fn (has-required-values? :parameter required-supply-parameters)
        :error/path [:header :parameter]}
       (no-error? (has-required-values? :parameter required-supply-parameters))]]]

    [:supply-profiles
     [:map
      [:header [:map
                {:error/message "column missing"}
                [:day-type string?]
                [:interval string?]
                [:grid-offer string?]]]
      [:rows [:sequential [:map
                           [:day-type string?]
                           [:interval number?]
                           [:grid-offer number?]
                           [:spreadsheet/row int?]]]]]]]

   [:fn
    {:error/fn rows-per-day-type
     :error/path [:supply-day-types :header :name]}
    (no-error? rows-per-day-type)]

   [:fn
    {:error/fn intervals-per-day-type
     :error/path [:supply-day-types :header :name]}
    (no-error? intervals-per-day-type)]

   [:fn
    {:error/fn (references? [:supply-profiles :rows S/ALL :day-type] 
                            [:supply-day-types :rows S/ALL :name])
     :error/path [:supply-day-types :header :name]}
    (no-error? (references? [:supply-profiles :rows S/ALL :day-type] 
                            [:supply-day-types :rows S/ALL :name]))]

   [:fn
    {:error/fn (references? [:supply-day-types :rows S/ALL :name] 
                            [:supply-profiles :rows S/ALL :day-type])
     :error/path [:supply-profiles :header :day-type]}
    (no-error? (references? [:supply-day-types :rows S/ALL :name] 
                            [:supply-profiles :rows S/ALL :day-type]))]

   [:fn
    {:error/fn fuel-names-match?
     :error/path [:supply-plant :header :fuel]}
    (no-error? fuel-names-match?)]

   [:fn
    {:error/fn (references? [:supply-substations :rows S/ALL :name] 
                            [:supply-plant :rows S/ALL :substation]) 
     :error/path [:supply-plant :header :substation]}
    (no-error? (references? [:supply-substations :rows S/ALL :name]
                            [:supply-plant :rows S/ALL :substation]))]

   [:fn
    {:error/fn profile-substation-names-match?
     :error/path [:supply-profiles :header :substation]}
    (no-error? profile-substation-names-match?)]

   [:fn
    {:error/fn default-heat-profile-matches?
     :error/path [:supply-parameters :header :default-profile]}
    (no-error? default-heat-profile-matches?)]])

(defn validate-spreadsheet
  "Coerce and validate a spreadsheet.
   
   Returns the coerced input with an extra key, :import/errors,
   which is either nil or an error map."
  [ss]
  (let [coercions (mt/transformer mt/string-transformer
                                  number-to-double-transformer
                                  number-to-boolean-transformer)
        schema (-> spreadsheet-schema
                   (schematise-dynamic-cols ss :pipe-costs number?)
                   (schematise-dynamic-cols ss :supply-profiles number?))

        coerced (m/decode schema ss coercions)
        errors (me/humanize (m/explain schema coerced))]
    (assoc coerced :import/errors errors)))



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
    (validate-spreadsheet in-ss)))

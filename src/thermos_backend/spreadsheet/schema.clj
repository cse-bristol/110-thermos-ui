(ns thermos-backend.spreadsheet.schema
  (:require [malli.provider :as mp]
            [malli.core :as m]
            [malli.error :as me]
            [malli.transform :as mt]
            [com.rpl.specter :as S]
            [clojure.string :as string]))

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
                          [:period {:optional true} number?]
                          [:rate {:optional true} double?]]]]]]

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

(defn- no-error?
  "Wrap a function in the format expected by malli's `:error/fn`, 
   and turn it into a function suitable for  malli's `:fn`, 
   which returns false if non-nil or true if error string is nil."
  [f]
  (fn [arg] (nil? (f {:value arg} nil))))

(defn- duplicates 
  "Get the duplicate items in a collection"
  [coll]
  (->> coll
       (frequencies)
       (filter (fn [[_ freq]] (> freq 1)))
       (map first)))

(defn- substations-unique? [{supply-substations :value} _]
  (let [substations
        (for [substation (:rows supply-substations)] (:name substation))

        dups (duplicates substations)]

    (when (seq dups)
      (str "substation names not unique: '" (string/join "', '" dups) "'"))))

(defn- day-types-unique? [{supply-day-types :value} _]
  (let [day-types
        (for [day-type (:rows supply-day-types)] (:name day-type))

        dups (duplicates day-types)]

    (when (seq dups)
      (str "day-type names not unique: '" (string/join "', '" dups) "'"))))

(defn- profile-substation-names-match?
  "Return an error message if the names of substation profile columns do not
   match those defined in supply-substations.
   
  Allows the load-kw profile column to be missing."
  [{{:keys [supply-profiles supply-substations]} :value} _]
  (let [substations
        (-> (for [substation (:rows supply-substations)] (:name substation))
            (set))

        profile-cols
        (for [profile (vals (:header supply-profiles))
              :when (string/starts-with? (name profile) "Substation: ")] profile)

        all-not-ok
        (for [s profile-cols
              :let [substation (string/replace-first s "Substation: " "")]
              :when (not (contains? substations substation))]
          substation)]
    
    (case (count (set all-not-ok))
      0 nil
      1 (str "substation '" (first all-not-ok) "' referenced, "
             "but not defined in sheet 'Supply substations'")
      (str "substations '" (string/join "', '" (set all-not-ok)) "' referenced, "
           "but not defined in sheet 'Supply substations'"))))

(defn- plant-substation-names-match?
  "Return an error message if the names of substations referenced in supply-plant
   do not match those defined in supply-substations."
  [{{:keys [supply-plant supply-substations]} :value} _]
  (let [plant-substations
        (for [plant (:rows supply-plant)
              :let [s (:substation plant)]
              :when (some? s)] s)

        substations
        (-> (for [substation (:rows supply-substations)] (:name substation))
            (set))

        all-not-ok
        (for [s plant-substations
              :when (not (contains? substations s))] s)]
    
    (case (count (set all-not-ok))
      0 nil
      1 (str "substation '" (first all-not-ok) "' referenced, "
             "but not defined in sheet 'Supply substations'")
      (str "substations '" (string/join "', '" (set all-not-ok)) "' referenced, "
           "but not defined in sheet 'Supply substations'"))))

(defn- fuel-names-match?
  "Return an error message if the names of fuel profile columns do not match 
   the fuels referenced in supply-plant.
   
   Allows the CO₂, NOₓ and PM₂₅ columns to be missing."
  [{{:keys [supply-profiles supply-plant]} :value} _]
  (let [fuels
        (for [plant (:rows supply-plant)]
          (:fuel plant))

        profile-cols
        (-> (for [profile (vals (:header supply-profiles))
                  :when (string/starts-with? (name profile) "Fuel: ")] profile)
            (set))

        all-not-ok
        (for [fuel fuels
              :when (not (contains? profile-cols (str "Fuel: " fuel " price")))] fuel)]
    
    (case (count (set all-not-ok))
      0 nil
      1 (str "fuel '" (first all-not-ok) "' referenced, "
             "but pricing not defined in sheet 'Supply profiles'")
      (str "fuels '" (string/join "', '" (set all-not-ok)) "' referenced, "
           "but pricing not defined in sheet 'Supply profiles'"))))

(defn- day-type-names-match?
  "Return an error message if the names of day-types supply-day-types
   do not match the day-type names in supply-profiles"
  [{{:keys [supply-profiles supply-day-types]} :value} _]
  (let [profiles
        (->> (:rows supply-profiles)
             (group-by :day-type))
        
        all-not-ok
        (for [day-type (:rows supply-day-types)
              :when (not (contains? profiles (:name day-type)))] (:name day-type))]
    
    (case (count (set all-not-ok))
      0 nil
      1 (str "day type '" (first all-not-ok) "' defined, "
             "but no prices set in sheet 'Supply profiles'")
      (str "day types '" (string/join "', '" (set all-not-ok)) "' defined, "
           "but no prices set in sheet 'Supply profiles'"))))

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
                    num-profile-rows (count (get profiles (:name day-type)))
                    profile-rows (map (fn [row] (double (:interval row))) (get profiles (:name day-type)))]
              :when (or (not= (int divisions) num-profile-rows)
                        (not= (range 0.0 divisions) profile-rows))]
          (str "day type '" (:name day-type) "' has "
               (int divisions) " divisions, "
               "but had " num-profile-rows " entries in sheet 'supply profiles'"))]

    (when (seq all-not-ok)
      (string/join ". " all-not-ok))))

(def supply-model-schema
  [:and
   [:map
    {:error/message "missing sheet from spreadsheet"}
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
       {:error/fn day-types-unique?
        :error/path [:header :name]}
       (no-error? day-types-unique?)]]]

    [:supply-storage
     [:map
      [:header [:map
                {:error/message "column missing"}
                [:name string?]
                [:lifetime string?]
                [:capacity string?]
                [:efficiency string?]
                [:fixed-capex string?]
                [:capex-per-kwp string?]
                [:capex-per-kwh string?]]]
      [:rows [:sequential [:map
                           [:name string?]
                           [:lifetime number?]
                           [:capacity number?]
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
                 [:headroom string?]
                 [:alpha string?]]]
       [:rows [:sequential [:map
                            [:name string?]
                            [:headroom number?]
                            [:alpha double?]
                            [:spreadsheet/row int?]]]]]
      [:fn
       {:error/fn substations-unique?
        :error/path [:header :name]}
       (no-error? substations-unique?)]]]

    [:supply-objective
     [:map
      [:header [:map
                {:error/message "column missing"}
                [:accounting-period string?]
                [:discount-rate string?]
                [:curtailment-cost string?]
                [:can-dump-heat string?]
                [:mip-gap string?]
                [:time-limit string?]]]
      [:rows [:sequential [:map
                           [:accounting-period number?]
                           [:discount-rate double?]
                           [:curtailment-cost number?]
                           [:can-dump-heat boolean?]
                           [:mip-gap double?]
                           [:time-limit number?]
                           [:spreadsheet/row int?]]]]]]

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
                           [:grid-offer number?]]]]]]]

   [:fn
    {:error/fn rows-per-day-type
     :error/path [:supply-day-types]}
    (no-error? rows-per-day-type)]

   [:fn
    {:error/fn day-type-names-match?
     :error/path [:supply-day-types]}
    (no-error? day-type-names-match?)]

   [:fn
    {:error/fn fuel-names-match?
     :error/path [:supply-plant]}
    (no-error? fuel-names-match?)]

   [:fn
    {:error/fn plant-substation-names-match?
     :error/path [:supply-plant]}
    (no-error? plant-substation-names-match?)]

   [:fn
    {:error/fn profile-substation-names-match?
     :error/path [:supply-profiles]}
    (no-error? profile-substation-names-match?)]])

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
  (let [coercions (mt/transformer mt/string-transformer number-to-double-transformer
                                  number-to-boolean-transformer)

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

        pc-errors  (me/humanize (m/explain variable-pipe-costs-schema coerced-variable-pcs))]


    (assoc coerced :import/errors (merge-errors (when pc-errors {:pipe-costs pc-errors}) errors))))

(defn validate-supply-model-ss [ss]
  (let [coercions (mt/transformer mt/string-transformer
                                  number-to-double-transformer
                                  number-to-boolean-transformer)

        coerced (m/decode supply-model-schema ss coercions)
        errors (me/humanize (m/explain supply-model-schema coerced))]
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
    (validate-network-model-ss in-ss)))

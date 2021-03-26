(ns thermos-test.backend.spreadsheet.supply-model
  (:require [clojure.test :refer :all]
            [thermos-specs.defaults :as defaults]
            [thermos-backend.spreadsheet.core :as ss-core]
            [thermos-backend.spreadsheet.common :as ss-common]
            [thermos-backend.spreadsheet.supply-model :as supply-model]
            [clojure.java.io :as io]
            [thermos-specs.supply :as supply]
            [clojure.set :as set]
            [clojure.data :as data]))

(defn flat [x]
       (let [vals (vec (repeat 24 x))]
         {0 vals 1 vals 2 vals 3 vals 4 vals}))

(def initial-doc
  (merge (assoc-in defaults/default-document [:thermos-specs.supply/substations 0 :load-kw] (flat 1.23))
         #:thermos-specs.document
          {:pumping-emissions {:co2 0.1 :pm25 0.2 :nox 0.3}
           :emissions-cost {:co2 0.4 :pm25 0.5 :nox 0.6}
           :emissions-limit {:co2 {:value 0.9 :enabled true}
                             :pm25 {:value 0.8 :enabled true}
                             :nox {:value 0.7 :enabled true}}}))

(defn write-to-ss [doc]
  (let [out-sheet (ss-core/to-spreadsheet doc)
        baos (java.io.ByteArrayOutputStream.)]
    (ss-common/write-to-stream out-sheet baos)
    (.toByteArray baos)))

(defn read-to-doc [bytearray]
  (let [in (-> bytearray
               (io/input-stream)
               (ss-core/from-spreadsheet))]
    (when (:import/errors in) (println (:import/errors in)))
    in))

(defn =? [o1 o2 & {:keys [key exclude-keys] }]
  (let [val
        (cond
          (and (map? o1) (map? o2))
          (every? true? (for [key (set/union (set (keys o1)) (set (keys o2)))
                              :when (not (contains? exclude-keys key))]
                          (=? (get o1 key) (get o2 key) :key key :exclude-keys exclude-keys)))

          (and (sequential? o1) (sequential? o2))
          (and (= (count o1) (count o2))
               (every? true? 
                       (map-indexed 
                        (fn [i item] 
                          (=? item (get o2 i) :key i :exclude-keys exclude-keys)) o1)))

          (and (number? o1) (number? o2))
          (== o1 o2)

          :else
          (= o1 o2))]
    (when-not val (println (str "difference at: " key) (data/diff o1 o2)))
    val))

(deftest basic-round-trip
  (let [round-tripped (-> initial-doc write-to-ss read-to-doc)]
    (is (= nil (:import/errors round-tripped)))
    (is (=? (::supply/plants round-tripped) (::supply/plants initial-doc) :exclude-keys #{:fuel}))
    (is (=? (::supply/day-types round-tripped) (::supply/day-types initial-doc)))
    (is (=? (::supply/storages round-tripped) (::supply/storages initial-doc)))
    (is (=? (::supply/grid-offer round-tripped) (::supply/grid-offer initial-doc)))
    (is (=? (::supply/substations round-tripped) (::supply/substations initial-doc)))
    (is (=? (::supply/objective round-tripped) (::supply/objective initial-doc)))))

(deftest heat-profiles
  (let [round-tripped
        (-> initial-doc write-to-ss read-to-doc)

        heat-profile
        (fn [doc day-type profile-name]
          (let [day-type-id
                (get (supply-model/id-lookup (::supply/day-types doc)) day-type)
                profile-id
                (get (supply-model/id-lookup (::supply/heat-profiles doc)) profile-name)]
            (get-in doc [::supply/heat-profiles profile-id day-type-id])))]

    (doseq [day-type ["Normal weekday" "Normal weekend" "Winter weekday" "Winter weekend" "Peak day"]
            profile ["Residential" "Commercial" "Flat"]]
      (is (= (heat-profile round-tripped day-type profile)
             (heat-profile initial-doc day-type profile))))))

(deftest fuels
  (let [round-tripped
        (-> initial-doc write-to-ss read-to-doc)

        fuel-price
        (fn [doc fuel-name]
          (let [fuel-id (get (supply-model/id-lookup (::supply/fuels doc)) fuel-name)]
            (get-in doc [::supply/fuels fuel-id :price])))

        fuel-name
        (fn [doc fuel-id]
          (get-in doc [::supply/fuels fuel-id :name]))

        plant-fuel
        (fn [doc plant-id]
          (fuel-name doc (get-in doc [::supply/plants plant-id :fuel])))]

    (is (= (fuel-price round-tripped "Electricity") (fuel-price initial-doc "Electricity")))
    (is (= (fuel-price round-tripped "Natural gas") (fuel-price initial-doc "Natural gas")))
    (is (= (plant-fuel round-tripped 0) (plant-fuel initial-doc 0) "Natural gas"))
    (is (= (plant-fuel round-tripped 1) (plant-fuel initial-doc 1) "Natural gas"))
    (is (= (plant-fuel round-tripped 2) (plant-fuel initial-doc 2) "Electricity"))))

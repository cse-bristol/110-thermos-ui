;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-util.converter
  (:require [thermos-specs.document :as document]
            [thermos-specs.demand :as demand]
            [thermos-specs.path :as path]
            [thermos-specs.supply :as supply]
            [thermos-specs.candidate :as candidate]
            [clojure.string :as string]
            [clojure.test :as test]
            [thermos-util :refer [assoc-by]]
            [cljts.core :as jts]
            
            #?@(:clj [[clojure.data.json :as json]]))
  #?(:clj
     (:import [org.locationtech.jts.geom Geometry])))

(defn- process-key*
  {:test #(do (test/is (= "candidate/id" (process-key* ::candidate/id)))
              (test/is (= "thing" (process-key* :thing))))}
  [key]
  (cond
    (string? key) key
    (keyword? key)
    (let [key-name (name key)
          key-ns   (namespace key)]
      (str
       (if key-ns
         (str (last (string/split key-ns #"\.")) "/")
         "")
       key-name))
    :else
    (str key)))

(def process-key (memoize process-key*))

(defn- to-indexed-seqs [coll]
  (if (map? coll)
    coll
    (map vector (range) coll)))

(defn- flatten-path [path step]
  (if (coll? step)
    (->> step
         to-indexed-seqs
         (map (fn [[k v]] (flatten-path (conj path k) v)))
         (into {}))
    [path step]))

(defn candidate-geometry [candidate]
  #?(:clj
     (let [geom (::candidate/geometry candidate)]
       (cond
         (string? geom)
         (json/read-str geom)

         (instance? Geometry geom)
         (jts/geom->map geom)

         :else geom))
     
     :cljs (js/JSON.parse (::candidate/geometry candidate))))


(defn network-candidate->geojson [candidate]
  (let [geometry (candidate-geometry candidate)
        other (dissoc candidate ::candidate/geometry)
        kvs (flatten-path [] other)

        json-value (fn [value]
                     (if (or (string? value)
                             (boolean? value)
                             (nil? value)
                             (and (number? value)
                                  #?(:clj (Double/isFinite value)
                                     :cljs (js/isFinite value))))
                       value
                       (str value)))]
    
    {:type :Feature
     :geometry geometry
     :properties
     (into {} (for [[p v] kvs]
                [(string/join " " (map process-key p))
                 (json-value v)]))}))

(defn network-problem->geojson [document]
  (let [candidates (::document/candidates document)]
    {:type :FeatureCollection
     :features
     (for [[candidate-id candidate] candidates]
       (assoc
        (network-candidate->geojson candidate)
        :id candidate-id))}))

;; We need to be able to send the properties backwards and forwards
;; most are pairs

(defn- f1 [f] (fn [x _] (f x)))

(def common-properties
  [[::candidate/inclusion   "inclusion" (f1 name) (f1 keyword)]
   [::candidate/user-fields "user_fields"]])

(def building-properties
  [[::demand/kwh   "annual_kwh"   ]
   [::demand/kwp   "peak_kw"      ]
   [::demand/group "demand_group" ]])

(def path-properties
  [[::path/civil-cost-id    "civil_cost"
    (fn [id d]
      (document/civil-cost-name d id))
    (fn [nm d]
      (document/civil-cost-by-name d nm))]
   
   [::path/maximum-diameter "max_diameter_mm"
    (f1 #(and % (* % 1000.0)))
    (f1 #(and % (/ % 1000.0)))]
   
   [::path/exists           "path_exists"     (f1 boolean)       (f1 boolean)]])

(def supply-properties
  [[::supply/capacity-kwp      "supply_capacity_kw"       ]
   [::supply/fixed-cost        "supply_capex_fixed"       ]
   [::supply/opex-per-kwp      "supply_opex_per_kw"       ]
   [::supply/cost-per-kwh      "supply_cost_per_kwh"      ]
   [::supply/capex-per-kwp     "supply_capex_per_kwp"     ]
   [::supply/emissions-factors "supply_emissions_factors" ]])

(defn properties-> [properties document candidate output]
  (reduce
   (fn [a [internal-key output-key write-fn _]]
     (let [write-fn (or write-fn (fn [x _] x))
           value (get candidate internal-key)]
       (assoc a output-key (write-fn value document))))
   output properties))

(defn properties<- [properties document input candidate]
  (reduce
   (fn [a [internal-key output-key _ read-fn]]
     (let [read-fn (or read-fn (fn [x _] x))
           value (get input output-key)]
       (cond-> a
         (contains? input output-key)
         (assoc internal-key (read-fn value document)))))
   candidate properties))

(defn- candidate-properties [document candidate]
  (as-> {} out
    (properties-> common-properties document candidate out)
    (cond->> out
      (candidate/is-building? candidate)
      (properties-> building-properties document candidate))
    (cond->> out
      (candidate/is-path? candidate)
      (properties-> path-properties document candidate))
    (cond->> out
      (candidate/has-supply? candidate)
      (properties-> supply-properties document candidate))))

(defn network-problem->geojson-2 [document]
  (let [candidates (::document/candidates document)]
    {:type :FeatureCollection
     :features
     (for [[candidate-id candidate] candidates]
       {:type :Feature
        :geometry (candidate-geometry candidate)
        :properties (candidate-properties document candidate)
        :id candidate-id})}))

(defn get-features [geojson]
  (case (name (:type geojson (get geojson "type")))
    "FeatureCollection"
    (into [] (mapcat get-features (:features geojson (get geojson "features"))))
    "Feature"
    [(assoc (get geojson "properties")
            "id" (get geojson "id"))]))

(defn update-from-geojson-2
  "Copy modified fields from features in the given geojson which have a matching ID
  back onto the candidates in document."
  [document geojson]
  (let [features (-> (get-features geojson)
                     (assoc-by #(get % "id")))]
    (document/map-candidates
     document
     (fn [c]
       (if-let [feature (get features (::candidate/id c))]
         (cond->> c
           true
           (properties<- common-properties document feature)
           
           (candidate/is-building? c)
           (properties<- (concat supply-properties building-properties)
                         document feature)

           (candidate/is-path? c)
           (properties<- path-properties document feature))
         c)))))


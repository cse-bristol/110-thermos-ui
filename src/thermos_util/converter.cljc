(ns thermos-util.converter
  (:require [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [clojure.string :as string]

            #?@(:clj [[clojure.data.json :as json]])))

(defn- process-key
  {:test #(do (assert (= "candidate-id" (process-key ::candidate/id)))
              (assert (= "thing" (process-key :thing))))}
  [key]
  (cond
    (string? key) key
    (keyword? key)
    (let [key-name (name key)
          key-ns   (namespace key)]
      (str
       (if key-ns
         (str (last (string/split key-ns #"\.")) "-")
         "")
       key-name))
    :else key))

(defn- process-value [value]
  (if (or (string? value)
          (boolean? value)
          (nil? value)
          (and (number? value)
               #?(:clj (Double/isFinite value)
                  :cljs (js/isFinite value))))
    value
    (str value)))

(defn- process-properties
  "Recursively flatten map of properties and simplify column names"
  {:test #(do (assert (= (process-properties {:this/that 1}) {"this-that" 1}))
              (assert (= (process-properties {:x {:y 1}}) {"x-y" 1})))}  
  ([properties] (process-properties properties ""))
  ([properties prefix]
   (reduce-kv
    (fn [a k v]
      (let [k (str prefix (process-key k))]
        (if (map? v)
          (merge a (process-properties v (str k "-")))
          (assoc a k (process-value v)))))
    {} properties)))

(defn network-candidate->geojson [candidate]
  (let [geometry (#?(:clj json/read-str
                     :cljs js/JSON.parse) (::candidate/geometry candidate))
        other (dissoc candidate ::candidate/geometry)]
    {:type :Feature
     :geometry geometry
     :properties (process-properties other)}))

(defn network-problem->geojson [document]
  (let [candidates (::document/candidates document)]
    {:type :FeatureCollection
     :features
     (for [[candidate-id candidate] candidates]
       (assoc
        (network-candidate->geojson candidate)
        :id candidate-id))}))


(ns thermos-util.converter
  (:require [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [clojure.string :as string]
            [clojure.test :as test]
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
    :else key))

(def process-key (memoize process-key*))

(defn- process-value [value]
  (cond
    (or (string? value)
        (boolean? value)
        (nil? value)
        (and (number? value)
             #?(:clj (Double/isFinite value)
                :cljs (js/isFinite value))))
    value

    (or (seq? value)
        (set? value)
        (vector? value))
    (vec (doall (map process-value value)))
    
    :else (str value)))

(defn- process-properties
  "Recursively flatten map of properties and simplify column names"
  {:test #(do (assert (= (process-properties {:this/that 1}) {"this-that" 1}))
              (assert (= (process-properties {:x {:y 1}}) {"x:y" 1})))}  
  ([properties] (process-properties properties ""))
  ([properties prefix]
   (reduce-kv
    (fn [a k v]
      (let [k (str prefix (process-key k))]
        (if (map? v)
          (merge a (process-properties v (str k ":")))
          (assoc a k (process-value v)))
        ))
    {} properties)))

;; this is a terrible thing which I do here
;; it would be preferable to convert the geometry straight into json in the writer.
(defn network-candidate->geojson [candidate]
  (let [geometry #?(:clj
                    (let [geom (::candidate/geometry candidate)]
                      (cond
                        (string? geom)
                        (json/read-str geom)

                        (instance? Geometry geom)
                        (jts/geom->map geom)

                        :else geom))
                    
                    :cljs (js/JSON.parse (::candidate/geometry candidate)))
        other (dissoc candidate ::candidate/geometry)]
    
    {:type :Feature
     :geometry geometry
     :properties (process-properties other)})
  )

(defn network-problem->geojson [document]
  (let [candidates (::document/candidates document)]
    {:type :FeatureCollection
     :features
     (for [[candidate-id candidate] candidates]
       (assoc
        (network-candidate->geojson candidate)
        :id candidate-id))}))


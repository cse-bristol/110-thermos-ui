(ns thermos-cli.output
  (:require [thermos-importer.util :refer [file-extension]]
            [clojure.pprint :refer [pprint]]
            [thermos-util.converter :as converter]
            [cljts.core :as jts]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [thermos-specs.document :as document]
            [thermos-specs.solution :as solution]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.path :as path])
  (:import [org.locationtech.jts.geom Geometry Point]))

(def ^:dynamic *identifier* nil)

(defn- output-extension [instance output-path]
  (keyword (file-extension output-path)))

(defmethod print-method org.locationtech.jts.geom.Geometry
  [this w]
  (.write w "#geojson ")
  (print-method (jts/geom->json this) w))

(defmulti save-state   output-extension)

(defmethod save-state :default [instance path]
  (throw (ex-info "Don't know how to write this output" {:path path})))

(defn- output ^java.io.Closeable [thing]
  (if (= thing "-")
    (proxy [java.io.FilterWriter] [(io/writer System/out)]
      (close [] (proxy-super flush)))
    (io/writer (io/file thing))))

(defmethod save-state :edn
  [instance path]
  (with-open [w (output path)]
    (if (= path "-") 
      (pprint instance w)
      (binding [*out* w] (prn instance)))))

(defmethod save-state :geojson
  [instance path]

  (with-open [w (output path)]
    (-> instance
        (converter/network-problem->geojson)
        (json/write w :value-fn
                    (fn write-geojson-nicely [k v]
                      (if (instance? Geometry v)
                        (jts/geom->json v)
                        v))))))

(defmethod save-state :json
  [instance path]

  (with-open [w (output path)]
    (-> instance
        (converter/network-problem->geojson)
        (json/write w :value-fn
                    (fn write-geojson-nicely [k v]
                      (if (instance? Geometry v)
                        (jts/geom->json v)
                        v))))))

;; we want to be able to roll up the TSV file to make a useful summary
;; the summary needs to be able to tell us
;; - number of heating systems of each sort
;; - peak & base of each sort
;; - amount of insulation
;; - amount of pipework
;; - money figures???

(def building-header ["lon" "lat" "system" "kwh" "kwp" "insulation"])
(defn- building-rows [instance]
  (let [candidates (vals (::document/candidates instance))
        mode       (document/mode instance)]
    (for [c (filter candidate/is-building? candidates)]
      (let [geom       ^Geometry (::candidate/geometry c)
            centroid   (.getCentroid geom)
            lon        (.getX centroid)
            lat        (.getY centroid)
            system     (candidate/solution-description c)
            kwh-orig   (candidate/annual-demand c mode)
            kwh        (candidate/solved-annual-demand c mode)
            kwp        (candidate/solved-peak-demand c mode)
            insulation (- kwh-orig kwh)]
        [lon lat system kwh kwp insulation]))))

(def pipe-header ["lon" "lat" "length" "diameter" "kw" "civils"])
(defn- pipe-rows [instance]
  (let [candidates (vals (::document/candidates instance))
        mode       (document/mode instance)]
    (for [c (filter candidate/is-path? candidates)
          :when (candidate/in-solution? c)]
      (let [geom       ^Geometry (::candidate/geometry c)
            centroid   (.getCentroid geom)
            lon        (.getX centroid)
            lat        (.getY centroid)
            length     (::path/length c)
            diameter   (::solution/diameter-mm c)
            kw         (::solution/capacity-kw c)
            civils     (document/civil-cost-name instance (::path/civil-cost-id c))
            ]
        [lon lat length diameter kw civils]))))

(defn- tsv-write [row]
  (when *identifier*
    (print *identifier*)
    (print \tab))
  (doseq [x (interpose \tab row)] (print x))
  (println))

(defmethod save-state :tsv
  [instance path]

  (let [filename (.getName (io/file path))
        [header rows]
        (if (re-find #"pipe" filename)
          [pipe-header pipe-rows]
          [building-header building-rows])]
    
    (with-open [w (output path)]
      (binding [*out* w]
        (binding [*identifier* (and *identifier* "problem")]
          (tsv-write header))
        (doseq [row (rows instance)] (tsv-write row))))))

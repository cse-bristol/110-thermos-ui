;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-cli.output
  (:require [thermos-importer.util :refer [file-extension has-extension]]
            [clojure.pprint :refer [pprint]]
            [thermos-util.converter :as converter]
            [cljts.core :as jts]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [thermos-specs.demand :as demand]
            [thermos-specs.document :as document]
            [thermos-specs.solution :as solution]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.path :as path])
  (:import [org.locationtech.jts.geom Geometry Point]
           [java.util.zip GZIPOutputStream]))

(def ^:dynamic *problem-id* nil)
(def ^:dynamic *id-field* nil)

(defn- filename [x] (.getName (io/as-file x)))

(defn- output-type [instance output-path]
  (let [name      (filename output-path)
        extension (file-extension output-path)

        format (if (= extension "gz")
                 (keyword (file-extension (.substring name 0 (- (.length name) 3))))
                 (keyword extension))

        content (cond
                  (re-find #"pipe"    name) :pipes
                  (re-find #"summary" name) :summary
                  :else :default)

        format ;; some shorthands for format?
        (case format
          (:json :gjson :geojson) :json
          format)
        ]
    [content format]))

(comment
  (defmethod print-method org.locationtech.jts.geom.Geometry
    [this w]
    (.write w "#geojson ")
    (print-method (jts/geom->json this) w)))

(defmethod print-method org.locationtech.jts.geom.Geometry
  [this w]
  (print-method (jts/geom->json this) w))

(defmulti save-state   output-type)

(defmethod save-state :default [instance path]
  (throw (ex-info "Don't know how to write this output" {:path path})))

(defn- output ^java.io.Closeable [thing]
  (if (= thing "-")
    (proxy [java.io.FilterWriter] [(io/writer System/out)]
      (close [] (proxy-super flush)))
    (let [thing (io/file thing)
          parent (.getParentFile thing)]
      (when-not (.exists parent) (.mkdirs parent))
      (if (has-extension thing "gz")
        (io/writer (GZIPOutputStream. (io/output-stream (io/file thing))))
        (io/writer (io/file thing))))))

(defmethod save-state [:default :edn]
  [instance path]
  (with-open [w (output path)]
    (if (= path "-") 
      (pprint instance w)
      (binding [*out* w] (prn instance)))))

(defmethod save-state [:default :json]
  [instance path]

  (with-open [w (output path)]
    (-> instance

        ;; This only saves a tiny bit in the end.
        ;; (cond-> (has-extension path "gz")
        ;;   (document/map-candidates
        ;;    (fn [c] (update c ::candidate/geometry jts/centroid))))

        (converter/network-problem->geojson)
        (json/write w))))

(defn- tsv-write [row]
  (doseq [x (interpose \tab row)] (print x))
  (println))

(defmacro ndp
  "World's least efficient decimal place formatter"
  ([x]   `(ndp (double ~x) 2))
  ([x n] `(if ~x (format ~(format "%%.%df" n) (double ~x)) "")))

(defmethod save-state [:pipes :tsv]
  [instance path]
  (with-open [w (output path)]
    (binding [*out* w]
      (tsv-write
       (cond->
           ["lon" "lat" "length" "diameter" "kw" "civils" "capex" "losses" "diversity"]
         *problem-id* (conj "problem")
         *id-field* (conj "id")
         ))
      
      (let [candidates (vals (::document/candidates instance))
            mode       (document/mode instance)]
        (doseq [row (for [c (filter candidate/is-path? candidates)
                          :when (candidate/in-solution? c)]
                      (let [geom       ^Geometry (::candidate/geometry c)
                            centroid   (jts/centroid geom)
                            lon        (ndp (.getX centroid) 6)
                            lat        (ndp (.getY centroid) 6)
                            length     (ndp (::path/length c))
                            diameter   (ndp (::solution/diameter-mm c))
                            kw         (ndp (::solution/capacity-kw c) 0)
                            civils     (document/civil-cost-name instance (::path/civil-cost-id c))
                            capex      (ndp (:principal (::solution/pipe-capex c)))
                            losses     (ndp (::solution/losses-kwh c))
                            diversity  (ndp (::solution/diversity c) 3)
                            ]
                        (cond->
                            [lon lat length diameter kw civils
                             capex losses diversity]
                          *problem-id* (conj *problem-id*)
                          *id-field*   (conj (get c *id-field*))
                          )))]
          (tsv-write row))))))

(defmethod save-state [:default :tsv]
  [instance path]
  (with-open [w (output path)]
    (binding [*out* w]
      (tsv-write
       (cond-> ["lon" "lat" "system"
                "kwh" "kwp" "count"
                "insulation" "insulationarea" "insulationcapex"
                "systemcapex" "systemfuel" "systemopex" 
                "networkrevenue"
                "supplykwp" "supplycapex" "supplyfuel" "supplyopex" 
                ]
         *problem-id* (conj "problem")
         *id-field*   (conj "id")
         ))

      (let [candidates (vals (::document/candidates instance))
            mode       (document/mode instance)]
        (doseq [row (for [c (filter candidate/is-building? candidates)]
                      (let [geom       ^Geometry (::candidate/geometry c)
                            centroid   (jts/centroid geom)
                            lon        (ndp (.getX centroid) 6)
                            lat        (ndp (.getY centroid) 6)
                            system     (candidate/solution-description c)

                            kwh-orig   (candidate/annual-demand c mode)
                            kwh        (candidate/solved-annual-demand c mode)
                            insulation (ndp (- kwh-orig kwh) 0)

                            kwh-orig   (ndp kwh-orig 0)
                            kwh        (ndp kwh 0)

                            kwp        (ndp (candidate/solved-peak-demand c mode) 0)

                            syscapex   (ndp ;; capex of heatex or individual system
                                     (+ (-> c ::solution/alternative :capex (:principal 0))
                                        (-> c ::solution/connection-capex   (:principal 0)))
                                     0)
                            sysopex    (ndp (-> c ::solution/alternative :opex (:annual 0)) 0)
                            sysfuel   (ndp (-> c ::solution/alternative :heat-cost (:annual 0)) 0)
                            revenue (ndp (-> c ::solution/heat-revenue (:annual 0)) 0)
                            
                            ccount  (::demand/connection-count c 1)
                            skwp    (ndp (-> c (::solution/capacity-kw 0)) 0)
                            scapex  (ndp (-> c ::solution/supply-capex (:principal 0)) 0)
                            sopex   (ndp (-> c ::solution/supply-opex (:annual 0)) 0)
                            sheat   (ndp (-> c ::solution/heat-cost (:annual 0)) 0)
                            icapex  (ndp (reduce + 0 (keep :principal (::solution/insulation c))) 0)
                            iarea   (ndp (reduce + 0 (keep :area (::solution/insulation c))) 0)

                            system-pv-capex
                            (ndp ;; capex of heatex or individual system
                             (+ (-> c ::solution/alternative :capex (:present 0))
                                (-> c ::solution/connection-capex   (:present 0)))
                             0)

                            system-pv-opex
                            (ndp (+ (-> c ::solution/alternative :opex (:present 0))
                                    (-> c ::solution/alternative :heat-cost (:present 0))) 0)
                            
                            insulation-pv-capex
                            (ndp (reduce + 0 (keep :present (::solution/insulation c))) 0)
                            
                            ]
                        (cond-> [lon lat system
                                 kwh kwp ccount

                                 insulation iarea icapex
                                 syscapex sysfuel sysopex

                                 revenue
                                 skwp scapex sheat sopex
                                 ]
                          
                          *problem-id* (conj *problem-id*)
                          *id-field*   (conj (get c *id-field*))
                          )))]
          (tsv-write row))))))

(defmethod save-state [:summary :json]
  [instance path]
  (with-open [w (output path)]
    (json/write
     {:type :Feature
      :properties
      {:runtime    (::solution/runtime instance)
       :gap        (::solution/gap instance)
       :objective  (::solution/objective instance)
       :iterations (::solution/iterations instance)
       :status     (::solution/state instance)
       :log        (::solution/log instance)
       :problem    *problem-id*
       }
      :geometry
      (jts/geom->map
       (jts/convex-hull
        (for [c (vals (::document/candidates instance))]
          (::candidate/geometry c))))
      }
     w)))


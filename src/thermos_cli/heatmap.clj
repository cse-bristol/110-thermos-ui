;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-cli.heatmap
  "Tool for heat demand estimating only"
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]
            [clojure.string :as string]

            [thermos-importer.geoio :as geoio]
            [thermos-importer.lidar :as lidar]
            [thermos-backend.importer.process :as importer])
  
  (:gen-class))

(defn- conj-arg [m k v] (update m k conj v))
(def cli-options
  [["-l" "--lidar PATH"
    "The path to a LIDAR tile or directory of LIDAR tiles. Can repeat for multiple."
    :assoc-fn conj-arg]

   ["-i" "--input PATH"
    "The path to an input shapefile, geojson, or geopackage. Can repeat, get unioned."
    :assoc-fn conj-arg]

   ["-o" "--output PATH"
    "The path to the output file to produce. Tab separated values."]

   ["-d" "--degree-days N"
    "Degree days for heat demand estimator"
    :parse-fn #(Double/parseDouble %)
    :default (* 0.813 2501.0)]

   ["-k" "--key-field TEXT"
    "Field name for the identifier in the input shapefile."
    :default "toid"]

   [nil "--height-field TEXT"
    "Field name for fallback height. This will be interpreted as a number."
    :default "height"]

   [nil "--resi-field TEXT"
    "Field name for residential flag. This will be interpreted as a boolean."
    :default "resi"]

   [nil "--help" "This information here."]
   ])

(defn --main [{:keys
               [lidar input output degree-days
                key-field height-field resi-field]}]

  (let [inputs (geoio/read-from-multiple input :key-transform identity)
        tiles  (->> lidar
                    (map io/file)
                    (mapcat file-seq)
                    (filter #(and (.isFile %)
                                  (let [name (.getName %)]
                                    (or (.endsWith name ".tif")
                                        (.endsWith name ".tiff")))))
                    (lidar/rasters->index))

        inputs (and (seq inputs)
                    (update inputs ::geoio/features
                            (fn [features]
                              (map (fn [feature]
                                     (-> feature
                                         (assoc :residential (get feature resi-field))
                                         (assoc :fallback-height (get feature height-field))))
                                   features))))
        
        inputs  (and (seq inputs)
                     (lidar/add-lidar-to-shapes inputs tiles))

        sdd     (Math/sqrt degree-days)
        
        fields [key-field
                :annual-demand
                :peak-demand
                :sap-water-demand
                :demand-source
                ::lidar/floor-area
                ::lidar/height
                ::lidar/perimeter
                ::lidar/shared-perimeter
                ::lidar/storeys
                ::lidar/footprint
                ::lidar/wall-area
                ::lidar/party-wall-area
                ::lidar/external-wall-area
                ::lidar/external-surface-area
                ::lidar/volume
                ::lidar/ext-surface-proportion
                ::lidar/ext-surface-per-volume
                ::lidar/tot-surface-per-volume
                ::lidar/height-source
                ]
        tab-separate #(string/join ;; blank = nil?
                       (str \tab)
                       (for [x %] (if (nil? x) "" (str x))))
        ]
    (with-open [output (io/writer (io/file output))]
      (binding [*out* output]
        (println (tab-separate (map name fields)))
        (doseq [input (::geoio/features inputs)]
          (let [input (importer/produce-heat-demand input sdd)
                input (assoc input
                             :peak-demand
                             (importer/run-peak-model (:annual-demand input)))
                ]
            (println (tab-separate (for [f fields] (get input f))))))))))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (if (or (:help options) (seq errors))
      (do
        (println "SUMMARY:")
        (println summary))

      (--main options))))



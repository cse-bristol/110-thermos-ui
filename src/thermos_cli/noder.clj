;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-cli.noder
  "Tool for heat demand estimating only"
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]
            [clojure.tools.logging :as log]

            [thermos-importer.geoio :as geoio]
            [thermos-importer.spatial :as spatial])

  (:gen-class))

(defn- conj-arg [m k v] (update m k conj v))
(def cli-options
  [["-i" "--input PATH"
    "The path to an input shapefile, geojson, or geopackage. Can repeat, get unioned. They should all share the same CRS."
    :assoc-fn conj-arg]
   
   [nil "--building-out FILE" "The output shapefile, geojson or geopackage. Will contain all polygon and point geometries from inputs"
    :default "./noded-buildings.gpkg"]
   
   [nil "--path-out FILE" "The output shapefile, geojson or geopackage. Will contain all linestring geometries from inputs"
    :default "./noded-paths.gpkg"]

   [nil "--connect-to-connectors" "Allow connecting to connectors."
    :default false]
   
   [nil "--shortest-face LENGTH" "When finding face centers, only faces longer than this will be considered."
    :default 3.0
    :parse-fn #(Double/parseDouble %)]
   
   [nil "--snap-tolerance DIST" "Patch holes in the network of length DIST."
    :default nil
    :parse-fn #(Double/parseDouble %)]
   
   [nil "--trim-paths" "Remove paths that don't go anywhere."]

   [nil "--ignore-paths FIELD=VALUE*" "Ignore paths where FIELD=VALUE"
    :assoc-fn conj-arg
    :parse-fn #(string/split % #"=")]

   [nil "--help" "This information here."]])

(defn- line? [feature]
  (-> feature ::geoio/type #{:line-string :multi-line-string} boolean))

(defn node-connect
  "Given a set of shapes, do the noding and connecting dance"
  [{crs ::geoio/crs features ::geoio/features}

   {:keys [shortest-face
           snap-tolerance trim-paths
           transfer-field
           connect-to-connectors]
    :or {connect-to-connectors false}}]
  (when (seq features)
    (let [{lines true not-lines false}
          (group-by line? features)

          lines (spatial/node-paths lines :snap-tolerance snap-tolerance :crs crs)

          [buildings roads]
          (if lines
            (spatial/add-connections
             crs not-lines lines
             :copy-field (and transfer-field [transfer-field transfer-field])
             :shortest-face-length shortest-face
             :connect-to-connectors connect-to-connectors)
            [not-lines nil])]

      [(cond-> roads
         trim-paths
         (spatial/trim-dangling-paths buildings))
       buildings])))

(defn --main [options]
  (let [geodata (when (seq (:input options))
                  (geoio/read-from-multiple (:input options)
                                            :key-transform identity))
        
        crs (::geoio/crs geodata)

        geodata (cond-> geodata

                  (seq (:ignore-paths options))
                  (update ::geoio/features
                          (let [ignore-fields (:ignore-paths options)
                                ignored? (fn [x]
                                           (and (line? x)
                                                (some identity (for [[f v] ignore-fields] (= (get x f) v)))))]
                            (fn [features]
                              (let [out (remove ignored? features)]
                                (log/info
                                 "Ignored" (- (count features) (count out))
                                 "paths of" (count features) "geoms")
                                out)))))

        [paths buildings] (node-connect geodata options)]
    (geoio/write-to {::geoio/features paths ::geoio/crs crs} (:path-out options))
    (geoio/write-to {::geoio/features buildings ::geoio/crs crs} (:building-out options))))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (if (or (:help options) (seq errors))
      (do
        (println "SUMMARY:")
        (println summary))

      (--main options))))



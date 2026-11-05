;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.gpkg.upload-test
  (:require [thermos-backend.gpkg :as gpkg]
            [thermos-backend.gpkg.upload :as upload]
            [thermos-backend.test-documents :as test-documents]
            [thermos-backend.content-migrations.piecewise :as piecewise]
            [next.jdbc :as jdbc]
            [clojure.edn :as edn]
            [clojure.test :as t]))

(def ^:dynamic *gpkg* nil)

(t/use-fixtures :each
  (fn [f]
    (let [file (java.io.File/createTempFile "thermos-test" ".gpkg")]
      (.delete file)
      (try (binding [*gpkg* file] (f))
           (finally (.delete file))))))

(def ^:private buildings
  [{:geoid "b1" :orig-id "osm-1"
    :geometry "{\"type\":\"Polygon\",\"coordinates\":[[[0,0],[1,0],[1,1],[0,0]]]}"
    :user-fields {"Use" "house" "Floors" 2.0 "ID" "clash"}
    :demand-kwh-per-year 1000.0 :demand-kwp 3.0 :connection-count 2
    :demand-source "svm-3d" :peak-source "model" :floor-area 80.0 :roof-area 40.0
    :conn-group 7 :connection-id ["p1" "p2"]}
   ;; points go in a layer of their own
   {:geoid "b2" :orig-id "osm-2"
    :geometry "{\"type\":\"Point\",\"coordinates\":[2,2]}"
    :demand-kwh-per-year 500.0 :demand-kwp 1.0 :connection-count 1
    :connection-id ["p1"]}])

(defn- write-example! []
  (let [w (gpkg/create *gpkg*)]
    (gpkg/add-network! w {:id 10 :name "solved" :created #inst "2026-01-02T03:04:05Z"}
                       test-documents/solved-document)
    (gpkg/add-network! w {:id 4 :name "unsolved"} test-documents/unsolved-document)
    (gpkg/add-map-info! w "My map" {:geometry {:source :osm}})
    (gpkg/add-map-features! w :buildings 2 [(first buildings)]
                            {"Use" :text "Floors" :real "ID" :text})
    (gpkg/add-map-features! w :buildings 0 [(second buildings)] {})
    (gpkg/add-map-features!
     w :paths 1
     [{:geoid "p1" :orig-id "osm-p" :start-id "j1" :end-id "j2" :length 150.0
       :geometry "{\"type\":\"LineString\",\"coordinates\":[[0,0],[1,1]]}"}]
     {})
    (gpkg/finish! w)))

(t/deftest contents
  (write-example!)
  (let [{:keys [name parameters layers feature-count networks without-documents]}
        (upload/read-contents *gpkg*)]
    (t/is (= "My map" name))
    (t/is (= {:geometry {:source :osm}} parameters))
    (t/is (= {:buildings ["map_buildings" "map_buildings_points"] :paths ["map_paths"]} layers))
    (t/is (= 3 feature-count))
    (t/testing "networks are in the order they were saved"
      (t/is (= ["unsolved" "solved"] (map :name networks)))
      (t/is (= [nil #inst "2026-01-02T03:04:05Z"] (map :created networks))))
    (t/is (= 0 without-documents))))

(t/deftest candidates
  (write-example!)
  (let [[b1] (upload/read-candidates *gpkg* :buildings "map_buildings")
        [b2] (upload/read-candidates *gpkg* :buildings "map_buildings_points")
        [p1] (upload/read-candidates *gpkg* :paths "map_paths")]
    (t/is (= {:geoid "b1" :orig-id "osm-1" :demand-kwh-per-year 1000.0 :demand-kwp 3.0
              :connection-count 2 :demand-source "svm-3d" :peak-source "model"
              :floor-area 80.0 :roof-area 40.0 :conn-group 7 :connection-id "p1,p2"
              :user-fields {"Use" "house" "Floors" 2.0 "ID" "clash"}}
             (dissoc b1 :geometry)))
    (t/testing "single geometries aren't multi"
      (t/is (re-matches #"POLYGON .*" (:geometry b1)))
      (t/is (= "POINT (2 2)" (:geometry b2))))
    (t/is (= {:geoid "p1" :orig-id "osm-p" :start-id "j1" :end-id "j2" :length 150.0
              :user-fields {} :geometry "LINESTRING (0 0, 1 1)"}
             p1))))

(t/deftest edited-candidates
  (write-example!)
  ;; as someone might in QGIS
  (with-open [c (jdbc/get-connection (str "jdbc:sqlite:" *gpkg*))]
    ;; the spatial index's triggers need functions QGIS has but sqlite hasn't
    (doseq [{:keys [name]} (jdbc/execute! c ["SELECT name FROM sqlite_master WHERE type = 'trigger'"]
                                          {:builder-fn next.jdbc.result-set/as-unqualified-maps})]
      (jdbc/execute! c [(str "DROP TRIGGER \"" name "\"")]))
    (jdbc/execute! c ["UPDATE map_buildings SET annual_demand_kwh = 1234, id = NULL"])
    (jdbc/execute! c ["UPDATE map_paths SET start = NULL"]))
  (let [[b1] (upload/read-candidates *gpkg* :buildings "map_buildings")]
    (t/is (== 1234 (:demand-kwh-per-year b1)))
    (t/testing "a building without an id gets one"
      (t/is (string? (:geoid b1)))))
  (t/testing "a path without ends is left out"
    (t/is (= [] (upload/read-candidates *gpkg* :paths "map_paths")))))

(t/deftest documents
  (write-example!)
  (let [{:keys [networks]} (upload/read-contents *gpkg*)
        by-name (into {} (for [n networks] [(:name n) (:network-id n)]))]
    (t/is (= test-documents/solved-document
             (edn/read-string (upload/read-document *gpkg* (by-name "solved")))))
    (t/is (= test-documents/unsolved-document
             (edn/read-string (upload/read-document *gpkg* (by-name "unsolved")))))

    (t/testing "a document from a newer THERMOS can't be read"
      (with-open [c (jdbc/get-connection (str "jdbc:sqlite:" *gpkg*))]
        (jdbc/execute! c ["UPDATE network_documents SET format_version = ?"
                          (inc piecewise/current-version)]))
      (t/is (thrown? Exception (upload/read-document *gpkg* (by-name "solved")))))))

(t/deftest not-a-thermos-gpkg
  (spit *gpkg* "not a geopackage")
  (t/is (= "This isn't a GeoPackage."
           (try (upload/read-contents *gpkg*)
                (catch Exception e (upload/problem-message e)))))
  (.delete *gpkg*)

  (t/testing "networks without the map's data"
    (let [w (gpkg/create *gpkg*)]
      (gpkg/add-network! w {:name "n"} test-documents/unsolved-document)
      (gpkg/finish! w))
    (t/is (re-find #"hasn't got the map's buildings and paths"
                   (try (upload/read-contents *gpkg*)
                        (catch Exception e (upload/problem-message e)))))))

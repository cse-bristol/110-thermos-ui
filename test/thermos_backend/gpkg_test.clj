;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.gpkg-test
  (:require [thermos-backend.gpkg :as gpkg]
            [thermos-backend.test-documents :as test-documents]
            [thermos-backend.spreadsheet.core :as spreadsheet]
            [geometry.gpkg :as reader]
            [geometry.gpkg.validate :as validate]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [clojure.test :as t])
  (:import [org.locationtech.jts.geom Geometry]))

(def ^:dynamic *gpkg* nil)

(defn- with-file [f]
  (let [file (java.io.File/createTempFile "thermos-test" ".gpkg")]
    (.delete file)
    (try (binding [*gpkg* file] (f))
         (finally (.delete file)))))

(t/use-fixtures :each with-file)

(defn- write! [& networks]
  (let [w (gpkg/create *gpkg*)]
    (doseq [[info doc & options] networks]
      (apply gpkg/add-network! w info doc options))
    w))

(defn- rows
  "The rows of `table`, as maps from column name to value"
  [table]
  (with-open [h (reader/open *gpkg* :table-name table)]
    (vec (reader/features h))))

(defn- column-types
  "The declared type of each column in `table`"
  [table]
  (with-open [c (jdbc/get-connection (str "jdbc:sqlite:" *gpkg*))]
    (->> (jdbc/execute! c [(format "PRAGMA table_info(\"%s\")" table)]
                        {:builder-fn rs/as-unqualified-lower-maps})
         (into {} (map (juxt :name :type))))))

(defn- column-names
  "The columns of `table`, in order"
  [table]
  (with-open [c (jdbc/get-connection (str "jdbc:sqlite:" *gpkg*))]
    (mapv :name (jdbc/execute! c [(format "PRAGMA table_info(\"%s\")" table)]
                                {:builder-fn rs/as-unqualified-lower-maps}))))

(defn- aliases
  "The human name gpkg_data_columns gives each column of `table`"
  [table]
  (into {} (for [[c m] (reader/column-metadata *gpkg* table)] [c (:name m)])))

(defn- by-id [rows] (into {} (for [r rows] [(get r "id") r])))

(defn- valid! []
  (t/is (= [] (validate/failures (str *gpkg*)))))

(t/deftest identifiers
  (t/is (= "annual_demand_kwh" (gpkg/identifier "Annual demand (kWh)")))
  (t/is (= "co2_kg_yr" (gpkg/identifier "CO₂ (kg/yr)")))
  (t/is (= "nox_npv" (gpkg/identifier "NOₓ (NPV ¤)")))
  (t/is (= "maximum_reduction_pct" (gpkg/identifier "Maximum reduction %")))
  (t/is (= "column" (gpkg/identifier "¤"))))

(t/deftest one-network
  (gpkg/finish!
   (write! [{:id 12 :name "My network" :author "Someone" :map-id 3 :map-name "A map"
             :created (java.util.Date. 0)}
            test-documents/solved-document
            :objective-trace [[0.5 13000.0] [2.0 12345.5]]]))
  (valid!)

  (t/testing "layers and tables"
    (t/is (= #{"buildings" "buildings_points" "paths" "supply_points"}
             (reader/table-names *gpkg* :spatial-only? true)))
    (t/is (= (into #{"buildings" "buildings_points" "paths" "supply_points"
                     "networks" "network_documents"
                     "solver" "solver_iterations" "objective_trace"}
                   (map (comp gpkg/identifier :name)
                        (spreadsheet/to-tables test-documents/solved-document)))
             (reader/table-names *gpkg*))))

  (t/testing "geometry"
    (t/is (= {:column-name "geom" :geometry-type :multi-polygon :srid 4326}
             (reader/geometry-column *gpkg* "buildings")))
    (t/is (= :multi-point (:geometry-type (reader/geometry-column *gpkg* "buildings_points"))))
    (t/is (= :multi-line-string (:geometry-type (reader/geometry-column *gpkg* "paths"))))
    (t/is (= :multi-point (:geometry-type (reader/geometry-column *gpkg* "supply_points"))))
    (let [buildings (by-id (rows "buildings"))
          [supply] (rows "supply_points")]
      (t/is (= #{"b1" "b2"} (set (keys buildings))))
      (t/is (= ["b3"] (map #(get % "id") (rows "buildings_points"))))
      (t/is (every? #(instance? Geometry (:geometry %)) (vals buildings)))
      (t/is (= "EPSG:4326" (:crs (buildings "b1"))))
      (t/is (< -2.591 (.getX (.getCoordinate ^Geometry (:geometry (buildings "b1")))) -2.589))
      (t/is (.contains ^Geometry (:geometry (buildings "b1")) ^Geometry (:geometry supply)))
      (t/is (= "b1" (get supply "id")))))

  (t/testing "buildings have their inputs and solution"
    (let [types (column-types "buildings")
          buildings (by-id (concat (rows "buildings") (rows "buildings_points")))]
      (t/is (= {"network_id" "INTEGER" "network" "TEXT" "id" "TEXT" "constraint" "TEXT"
                "annual_demand_kwh" "REAL" "peak_demand_kw" "REAL" "floors" "REAL"
                "heating_system" "TEXT" "revenue_yr" "REAL" "co2_kg_yr" "REAL"}
               (select-keys types ["network_id" "network" "id" "constraint"
                                   "annual_demand_kwh" "peak_demand_kw" "floors"
                                   "heating_system" "revenue_yr" "co2_kg_yr"])))
      (t/is (= "MULTIPOLYGON" (types "geom")))
      (t/is (= "Annual demand (kWh)" ((aliases "buildings") "annual_demand_kwh")))
      (t/is (= {"network_id" 1 "network" "My network" "annual_demand_kwh" 10000.0
                "heating_system" "network" "revenue_yr" 800.0 "address" "b2 High Street"}
               (select-keys (buildings "b2") ["network_id" "network" "annual_demand_kwh"
                                              "heating_system" "revenue_yr" "address"])))
      (t/is (= {"heating_system" "Gas boiler" "system_capex_principal" 2000.0
                "co2_kg_yr" 2000.0 "revenue_yr" nil}
               (select-keys (buildings "b3") ["heating_system" "system_capex_principal"
                                              "co2_kg_yr" "revenue_yr"])))))

  (t/testing "paths"
    (let [types (column-types "paths")
          paths (by-id (rows "paths"))]
      (t/is (= ["BOOLEAN" "BOOLEAN" "REAL" "REAL"]
               (map types ["exists" "in_solution" "diameter_mm" "length"])))
      (t/is (= {"in_solution" true "diameter_mm" 40.0 "capital_cost_principal" 30000.0
                "length" 70.5 "civils" "Hard"}
               (select-keys (paths "p1") ["in_solution" "diameter_mm" "capital_cost_principal"
                                          "length" "civils"])))
      (t/is (= true (get (paths "p2") "exists")))))

  (t/testing "tables"
    (let [[network] (rows "networks")]
      (t/is (= {"network_id" 1 "network" "My network" "thermos_id" 12 "map_id" 3 "map" "A map"
                "author" "Someone" "saved" "1970-01-01T00:00:00Z" "mode" "heating"
                "has_solution" true}
               (dissoc network :geometry :table :crs "fid" "project" "objective"))))
    (let [[solver] (rows "solver")]
      (t/is (= {"state" "valid" "runtime_s" 12.0 "objective" 12345.5 "gap" 0.01
                "lower_bound" 12300.0 "upper_bound" 12400.0 "iterations" 3
                "method" "heuristic, mip" "stopped_early" false}
               (select-keys solver ["state" "runtime_s" "objective" "gap" "lower_bound"
                                    "upper_bound" "iterations" "method" "stopped_early"]))))
    (t/is (= [[1 12000.0] [2 12345.5] [3 12345.5]]
             (map (juxt #(get % "iteration") #(get % "objective")) (rows "solver_iterations"))))
    (t/is (= [[0.5 13000.0] [2.0 12345.5]]
             (map (juxt #(get % "time_s") #(get % "objective")) (rows "objective_trace"))))
    (t/is (= "REAL" ((column-types "tariffs") "unit_rate_c")))
    (t/is (= "TEXT" ((column-types "other_parameters") "value"))))

  (t/testing "the tables are the spreadsheet's"
    (doseq [{tab :name columns :columns tab-rows :rows}
            (spreadsheet/to-tables test-documents/solved-document)
            :let [table (gpkg/identifier tab)
                  ;; the spreadsheet has some blank rows, for spacing
                  tab-rows (filter (fn [r] (some #(some? ((:key %) r)) columns)) tab-rows)]]
      (t/is (= (count tab-rows) (count (rows table))) tab)
      (t/is (= (map :name columns)
               (->> (column-names table)
                    (remove #{"fid" "network_id" "network"})
                    (map #((aliases table) % %))))
            tab)))

  (t/testing "the solution summary"
    (let [summary (rows "network_solution_summary")]
      (t/is (= "Whole system" (get (last summary) "item")))
      (t/is (= 33000.0 (get (first summary) "capital_cost"))))))

(t/deftest several-networks
  (let [w (write! [{:id 1 :name "unsolved"} test-documents/unsolved-document]
                  [{:id 2 :name "solved"} test-documents/solved-document])]
    (gpkg/finish! w))
  (valid!)

  (t/is (= [[1 "unsolved" 1 false] [2 "solved" 2 true]]
           (map (juxt #(get % "network_id") #(get % "network") #(get % "thermos_id") #(get % "has_solution"))
                (rows "networks"))))

  (t/testing "the networks share layers"
    (t/is (= {1 2 2 2} (frequencies (map #(get % "network_id") (rows "buildings")))))
    (t/is (= {1 2 2 2} (frequencies (map #(get % "network_id") (rows "paths")))))
    (t/is (= {1 1 2 1} (frequencies (map #(get % "network_id") (rows "supply_points")))))
    (t/is (= {1 21 2 21} (frequencies (map #(get % "network_id") (rows "pipe_costs"))))))

  (t/testing "columns are added when a network first has them"
    (let [paths (group-by #(get % "network_id") (rows "paths"))]
      (t/is (= "BOOLEAN" ((column-types "paths") "in_solution")))
      (t/is (every? #(nil? (get % "in_solution")) (paths 1)))
      (t/is (= #{true nil} (set (map #(get % "in_solution") (paths 2)))))
      (t/is (= "In solution" ((aliases "paths") "in_solution")))))

  (t/testing "only the run network has solver rows"
    (t/is (= [2] (map #(get % "network_id") (rows "solver"))))
    (t/is (= [] (rows "objective_trace"))))

  (t/testing "the extent is of all the networks"
    (let [[{:keys [min_x max_x]}]
          (with-open [c (jdbc/get-connection (str "jdbc:sqlite:" *gpkg*))]
            (jdbc/execute! c ["SELECT min_x, max_x FROM gpkg_contents WHERE table_name = 'paths'"]
                           {:builder-fn rs/as-unqualified-lower-maps}))]
      ;; the rtree index has floats
      (t/is (< (Math/abs (- -2.59 min_x)) 1e-6))
      (t/is (< (Math/abs (- -2.589 max_x)) 1e-6)))))

(t/deftest failed-network
  (let [w (write! [{:id 1 :name "first"} test-documents/unsolved-document])]
    ;; the spreadsheet's tables come after the network's row and layers
    (with-redefs [spreadsheet/to-tables (fn [_] (throw (ex-info "broken" {})))]
      (t/is (thrown? Exception (gpkg/add-network! w {:id 2 :name "broken"}
                                                  test-documents/solved-document))))
    (gpkg/add-network! w {:id 3 :name "last"} test-documents/solved-document)
    (gpkg/finish! w))
  (valid!)

  (t/testing "nothing of the failed network is left"
    (t/is (= [[1 "first"] [3 "last"]]
             (map (juxt #(get % "network_id") #(get % "network")) (rows "networks"))))
    (t/is (= #{1 3} (set (map #(get % "network_id") (rows "buildings")))))
    (t/is (= #{1 3} (set (map #(get % "network_id") (rows "paths")))))
    (t/is (= #{1 3} (set (map #(get % "network_id") (rows "pipe_costs")))))))

(t/deftest map-features
  (let [w (gpkg/create *gpkg*)
        row (fn [id geometry & {:as more}]
              (merge {:geoid id :orig-id (str "osm-" id) :geometry geometry} more))]
    (gpkg/add-map-features!
     w :buildings 2
     ;; lazy, as they would be from the database
     (map (fn [i]
            (row (str "b" i)
                 (format "{\"type\":\"Polygon\",\"coordinates\":[[[%s,0],[%s,0],[%s,1],[%s,0]]]}" i (inc i) i i)
                 :user-fields {"Use" "house" "Floors" i "ID" "clash" "fid" 1}
                 :demand-kwh-per-year 1000.0 :demand-kwp 3.0 :connection-count 1
                 :connection-id ["p1" "p2"]))
          (range 100))
     {"Use" :text "Floors" :real "ID" :text "fid" :real})
    (gpkg/add-map-features!
     w :paths 1
     [(row "p1" "{\"type\":\"LineString\",\"coordinates\":[[0,0],[1,1]]}"
           :start-id "j1" :end-id "j2" :length 150.0)]
     {})
    (gpkg/finish! w))
  (valid!)

  (let [buildings (rows "map_buildings")
        types (column-types "map_buildings")]
    (t/is (= 100 (count buildings)))
    (t/is (= {"id" "TEXT" "use" "TEXT" "floors" "REAL" "id_2" "TEXT" "fid_2" "REAL"
              "annual_demand_kwh" "REAL" "connections" "INTEGER" "geom" "MULTIPOLYGON"}
             (select-keys types ["id" "use" "floors" "id_2" "fid_2" "annual_demand_kwh"
                                 "connections" "geom"])))
    (t/is (= "ID 2" ((aliases "map_buildings") "id_2")))
    (t/is (= {"id" "b5" "original_id" "osm-b5" "use" "house" "floors" 5.0 "id_2" "clash"
              "connected_to" "p1,p2" "connections" 1}
             (select-keys (nth buildings 5) ["id" "original_id" "use" "floors" "id_2"
                                             "connected_to" "connections"]))))
  (t/is (= [{"id" "p1" "start" "j1" "end" "j2" "length_m" 150.0}]
           (map #(select-keys % ["id" "start" "end" "length_m"]) (rows "map_paths")))))

(t/deftest geometry-collections
  (gpkg/finish!
   (write! [{:name "collection"}
            (assoc-in test-documents/unsolved-document
                      [:thermos-specs.document/candidates "b2" :thermos-specs.candidate/geometry]
                      (str "{\"type\":\"GeometryCollection\",\"geometries\":["
                           "{\"type\":\"Polygon\",\"coordinates\":[[[0,0],[1,0],[1,1],[0,0]]]},"
                           "{\"type\":\"Point\",\"coordinates\":[5,5]}]}"))]))
  (valid!)
  (let [b2 (get (by-id (rows "buildings")) "b2")]
    (t/is (= "MultiPolygon" (.getGeometryType ^Geometry (:geometry b2))))
    (t/is (= 1 (.getNumGeometries ^Geometry (:geometry b2))))))

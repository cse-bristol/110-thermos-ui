;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.gpkg.upload
  "Uploading a GeoPackage written by thermos-backend.gpkg back into
  THERMOS, as a new map with its networks.

  The map is made from the map_buildings and map_paths layers (and
  their _points etc. variants), so any edits to those in QGIS are kept;
  a feature without an ID gets a new one. The networks come from
  network_documents, which has each network's document as it was saved,
  so they're restored exactly, but edits to the other layers and tables
  are ignored. Run logs and objective traces are lost."
  (:require ;; must come before geometry.gpkg; see its docstring
            [thermos-backend.gpkg.old-jts]
            [geometry.gpkg :as reader]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [thermos-backend.gpkg :as gpkg]
            [thermos-backend.db.maps :as maps]
            [thermos-backend.db.projects :as projects]
            [thermos-backend.content-migrations.piecewise :as piecewise]
            [thermos-specs.document :as document])
  (:import [org.locationtech.jts.geom Geometry GeometryCollection]))

(defn- problem
  "An exception whose message is for the person uploading `file`"
  [message]
  (ex-info message {::problem true}))

(defn problem-message
  "The message for the person uploading, if `e` is from `problem`"
  [e]
  (when (::problem (ex-data e)) (ex-message e)))

(defn- query [file sql]
  (with-open [c (jdbc/get-connection (str "jdbc:sqlite:" file))]
    (jdbc/execute! c sql {:builder-fn rs/as-unqualified-maps})))

(def ^:private column (memoize gpkg/identifier))

(defn- map-layers
  "The layers of map candidates of each kind among `tables`"
  [tables]
  (into {}
        (for [[kind base] [[:buildings "map_buildings"] [:paths "map_paths"]]]
          [kind (->> tables
                     (filter #(re-matches (re-pattern (str base "(_(points|lines|polygons))?")) %))
                     (sort))])))

(defn- table-count [file table]
  (:n (first (query file [(format "SELECT count(*) AS n FROM \"%s\"" table)]))))

(defn read-contents
  "What's in the GeoPackage `file`, which should be from
  thermos-backend.gpkg with the map's data in it:

  - :name and :parameters of the map, if it has them
  - :layers, the tables of map candidates by kind
  - :feature-count, how many candidates there are in all
  - :networks, the networks which have a document, in the order they
    were saved, each with its :network-id in the file, :name and
    :created time
  - :without-documents, how many networks haven't got a document

  If it isn't such a file, this throws an exception which
  `problem-message` explains."
  [file]
  (let [tables (try (set (map :table_name (query file ["SELECT table_name FROM gpkg_contents"])))
                    (catch Exception e
                      (log/info e "Reading uploaded GeoPackage")
                      (throw (problem "This isn't a GeoPackage."))))
        layers (map-layers tables)]
    (when (every? empty? (vals layers))
      (throw (problem
              (if (tables "networks")
                (str "This GeoPackage hasn't got the map's buildings and paths. "
                     "Download it from the project page with \"All the buildings "
                     "and paths on the map\" ticked.")
                "This isn't a GeoPackage downloaded from THERMOS."))))
    (let [[info] (when (tables "map") (query file ["SELECT * FROM map"]))
          networks (when (tables "networks")
                     (query file [(format "SELECT %s AS id, %s AS name, %s AS created, %s AS thermos_id FROM networks"
                                          (column "Network ID") (column "Network")
                                          (column "Saved") (column "THERMOS ID"))]))
          documented (if (tables "network_documents")
                       (set (map :id (query file [(format "SELECT %s AS id FROM network_documents"
                                                          (column "Network ID"))])))
                       #{})
          with-documents (filter (comp documented :id) networks)]
      {:name          (get info (keyword (column "Name")))
       :parameters    (some-> (get info (keyword (column "Parameters")))
                              (as-> s (try (edn/read-string s)
                                           (catch Exception e
                                             (log/warn e "Bad map parameters in GeoPackage")
                                             nil))))
       :layers        layers
       :feature-count (reduce + (for [ts (vals layers) t ts] (table-count file t)))
       :networks      (for [{:keys [id name created]}
                            (sort-by (juxt #(or (:thermos_id %) Long/MAX_VALUE) :id) with-documents)]
                        {:network-id id
                         :name       (or name (str "Network " id))
                         :created    (some-> created
                                             (as-> s (try (java.util.Date/from (java.time.Instant/parse s))
                                                          (catch Exception _ nil))))})
       :without-documents (- (count networks) (count with-documents))})))

;; The map

(def ^:private connected-to "Connected to")

(defn- user-field-name
  "The name of the user field in the column labelled `label`. A field
  with the same name as one of the map's own columns was written with a
  number after it (see gpkg/unique-labels), which this takes off."
  [label]
  (let [[_ base] (re-matches #"(.*) \d+" label)]
    (if (and base (or (gpkg/map-columns base) (= connected-to base)))
      base
      label)))

(defn- single
  "`g` as a single geometry, if it's a collection of one, as most
  THERMOS candidates are (the GeoPackage has only multi-geometries)"
  ^Geometry [^Geometry g]
  (if (and (instance? GeometryCollection g) (= 1 (.getNumGeometries g)))
    (.getGeometryN g 0)
    g))

(defn- feature-row
  "A row for maps/insert-into-map! from `feature` of a map layer whose
  columns have `labels`, or nil if it's got no geometry"
  [labels feature]
  (when-let [^Geometry g (some-> (:geometry feature) single)]
    (when-not (.isEmpty g)
      (reduce-kv
       (fn [row column label]
         (let [v (get feature column)]
           (cond
             (nil? v)               row
             (= connected-to label) (assoc row :connection-id v)
             (gpkg/map-columns label) (assoc row (gpkg/map-columns label) v)
             :else                  (assoc-in row [:user-fields (user-field-name label)] v))))
       {:geometry (.toText g) :user-fields {}}
       labels))))

(defn- with-defaults
  "`row` (from `feature-row`) with what the database needs of a
  candidate of `kind`, or nil if it can't be a candidate"
  [kind row]
  (let [row (update row :geoid #(or % (str (java.util.UUID/randomUUID))))
        row (update row :orig-id #(or (some-> % str) "unknown"))]
    (case kind
      :buildings
      (-> row
          (update :connection-id #(or % ""))
          (update :demand-kwh-per-year #(or % 0))
          (update :demand-kwp #(or % 0))
          (update :connection-count #(long (or % 1)))
          (update :conn-group #(some-> % long)))
      :paths
      (if (and (:start-id row) (:end-id row))
        (update row :length #(or % 0))
        (log/warn "Leaving out path" (:geoid row) "which hasn't got a start and end")))))

(defn read-candidates
  "The candidates of `kind` in map layer `table` of `file`, as rows for
  maps/insert-into-map!"
  [file kind table]
  (let [labels (into {} (for [[c m] (reader/column-metadata file table)
                              :when (:name m)]
                          [c (:name m)]))]
    (with-open [h (reader/open file :table-name table)]
      (->> (reader/features h)
           (keep #(some->> (feature-row labels %) (with-defaults kind)))
           (vec)))))

;; The networks

(defn read-document
  "The EDN of the document of network `network-id` in `file`, migrated to
  the current version if need be"
  ^String [file network-id]
  (let [[{:keys [version document]}]
        (query file [(format "SELECT %s AS version, %s AS document FROM network_documents WHERE %s = ?"
                             (column "Format version") (column "Document") (column "Network ID"))
                     network-id])
        version (or version 0)
        text    (gpkg/decode-document document)]
    (cond
      (> version piecewise/current-version)
      (throw (ex-info "The document is from a newer version of THERMOS" {:version version}))

      (< version piecewise/current-version)
      (-> (edn/read-string text) (piecewise/migrate version) (pr-str))

      :else text)))

(defn restore!
  "Fill map `map-id`, which should be new, with what's in the GeoPackage
  `file`: its candidates, and then its networks, which are saved as by
  `user-id`. A network that can't be read is left out. `progress` is
  the queue's progress function."
  [map-id file user-id progress]
  (let [{:keys [layers networks]} (read-contents file)
        n (count networks)]
    ;; the networks go first, as adding the candidates marks the map's
    ;; import completed
    (doseq [[i {:keys [network-id name created]}] (map-indexed vector networks)]
      (progress :message (str "Adding network " (inc i) " of " n)
                :percent (int (* 50 (/ i (max n 1)))))
      (try
        (let [content (read-document file network-id)]
          (projects/restore-network!
           user-id map-id
           {:name name :created created :content content
            :has-run (document/has-solution? (edn/read-string content))}))
        (catch Exception e
          (log/warn e "Leaving network" network-id name "out of map" map-id))))

    (progress :message "Reading buildings and paths" :percent 50)
    (let [read-kind (fn [kind] (vec (mapcat #(read-candidates file kind %) (kind layers))))
          buildings (read-kind :buildings)
          paths     (read-kind :paths)]
      (progress :message "Adding buildings and paths" :percent 70)
      (maps/insert-into-map!
       :map-id map-id
       ;; it's a new map, so there's nothing to erase
       :erase-geometry "POLYGON EMPTY"
       :buildings buildings
       :paths paths))))

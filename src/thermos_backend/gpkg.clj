;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.gpkg
  "Writing network documents, and the GIS data of a map, into a
  GeoPackage, using clj-geometry's geometry.gpkg.

  Several networks can go in one GeoPackage. They share its tables,
  each of which starts with a network_id column, and the networks table
  has a row about each network. So a GeoPackage of several networks
  has the same layers as one of one network, and in QGIS you can filter
  a layer to one network, or style it by network. The tables are:

  - networks: one row per network (its name, THERMOS id, map, author...)
  - buildings, paths and supply_points: feature layers of each network's
    candidates, with their inputs and any solution; the columns are those
    of the spreadsheet's buildings, paths and supply points sheets.
    Buildings whose geometry isn't polygonal go in buildings_points (or
    buildings_lines), and likewise for paths. Geometries are made multi
    (MultiPolygon etc.), so each layer has one geometry type.
  - the spreadsheet's other sheets (tariffs, pipe_costs,
    other_parameters, network_solution_summary, supply_plant...) as
    attribute tables, from thermos-backend.spreadsheet.core/to-tables.
  - solver, solver_iterations and objective_trace: how the optimiser did.
  - network_documents: each network's document, compressed, so the
    networks can be uploaded into THERMOS again (see
    thermos-backend.gpkg.upload). Edits to the other tables don't
    change them.
  - map_buildings and map_paths: all the map's candidates (the GIS data
    networks are made from), and map, about the map, if they are added
    (see `add-map-features!` and `add-map-info!`). With these the map can
    be uploaded again, with any edits to them.

  A table's columns are its sheet's, with names made into identifiers
  (\"Annual demand (kWh)\" is annual_demand_kwh), and the sheet's name
  for each kept as its name and title in gpkg_data_columns, which QGIS
  shows as the column's alias. Networks can have different columns (say
  for their pipe sizes), so a column is added to a table when a network
  first has it.

  Geometry in THERMOS is EPSG:4326 (geojson in documents, and in the
  candidates table)."
  (:require ;; must come before geometry.gpkg; see its docstring
            [thermos-backend.gpkg.old-jts]
            [geometry.gpkg :as gpkg]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [cljts.core :as jts]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [thermos-backend.spreadsheet.core :as spreadsheet]
            [thermos-backend.spreadsheet.network-model :as network-model]
            [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.solution :as solution]
            [thermos-backend.content-migrations.piecewise :as piecewise])
  (:import [org.locationtech.jts.geom Geometry GeometryFactory]
           [org.locationtech.jts.geom.util PointExtracter LineStringExtracter PolygonExtracter]
           [java.text Normalizer Normalizer$Form]))

(def srid
  "The CRS of THERMOS geometry"
  4326)

(def content-type "application/geopackage+sqlite3")

;; Names

(defn identifier
  "A table or column name for a sheet or column called `s`: lower case
  ascii letters, digits and underscores"
  [s]
  (let [s (-> (Normalizer/normalize (str s) Normalizer$Form/NFKD) ; CO₂ is CO2
              (string/lower-case)
              (string/replace "%" " pct ")
              (string/replace #"[^a-z0-9]+" "_")
              (string/replace #"^_+|_+$" ""))]
    (if (string/blank? s) "column" s)))

(def ^:private reserved-columns
  "Columns there are in tables other than those which come from data"
  #{"fid" "geom"})

(defn- unique-identifier [s used]
  (let [base (identifier s)]
    (->> (cons base (for [i (iterate inc 2)] (str base "_" i)))
         (remove used)
         (first))))

;; Values

(defn- infer-type
  "The column type for `values`, or `hint` (a spreadsheet column's
  :format) if they're all nil. Numbers are REAL, unless the hint says
  otherwise, since a column which is all whole numbers for one network
  may not be for the next."
  [hint values]
  (let [values (remove nil? values)]
    (cond
      (empty? values)       (case hint
                              (:double :real) :real
                              :integer        :integer
                              :boolean        :boolean
                              :text)
      (every? boolean? values) :boolean
      (every? number? values)  (if (= :integer hint) :integer :real)
      :else                    :text)))

(defn- sql-value
  "`v` as it goes in a column of `type`"
  [type v]
  (cond
    (nil? v)       nil
    (= :text type) (cond (string? v)  v
                         (keyword? v) (name v)
                         :else        (str v))
    (number? v)    (cond (not (Double/isFinite (double v))) nil
                         (= :integer type) (long v)
                         :else (double v))
    (boolean? v)   v
    (keyword? v)   (name v)
    (string? v)    v
    :else          (pr-str v)))

(defn- safe-key
  "A spreadsheet column's :key, which returns nil if it fails, as the
  spreadsheet shows ERR"
  [{key :key column :name}]
  (fn [row]
    (try (key row)
         (catch Exception e
           (log/warn "In GeoPackage column" column (.getMessage e))
           nil))))

;; Geometry

(defn- ->geometry ^Geometry [geometry]
  (cond
    (nil? geometry)                 nil
    (instance? Geometry geometry)   geometry
    (string? geometry)              (jts/json->geom geometry)
    (map? geometry)                 (jts/json->geom (json/write-str geometry))))

(defn candidate-geometry
  "The geometry of `candidate` as JTS, or nil if it has none, or it's bad"
  [candidate]
  (try (->geometry (::candidate/geometry candidate))
       (catch Exception e
         (log/warn "Bad geometry for" (::candidate/id candidate) (.getMessage e))
         nil)))

(defn- dimension
  "0 for points, 1 for lines and 2 for polygons; nil if `g` is nil or empty"
  [^Geometry g]
  (when (and g (not (.isEmpty g)))
    (.getDimension g)))

(def ^:private multi-types {0 :multi-point 1 :multi-line-string 2 :multi-polygon})

(defn- ->multi
  "`g` as a multi-geometry of its dimension, so a layer has only one type
  of geometry. Any parts of a collection with a lower dimension are lost,
  and an empty geometry is nil."
  [^Geometry g]
  (when-not (.isEmpty g)
    (let [^GeometryFactory f (.getFactory g)]
      (case (long (.getDimension g))
        0 (.createMultiPoint f (GeometryFactory/toPointArray (PointExtracter/getPoints g)))
        1 (.createMultiLineString f (GeometryFactory/toLineStringArray (LineStringExtracter/getLines g)))
        2 (.createMultiPolygon f (GeometryFactory/toPolygonArray (PolygonExtracter/getPolygons g)))))))

(defn- layer-name
  "The layer for geometry of dimension `dim`, where most of them have
  dimension `usual`. A collection goes by its highest dimension."
  [table usual dim]
  (cond
    (or (nil? dim) (= dim usual)) table
    :else (str table "_" (case (long dim) 0 "points" 1 "lines" 2 "polygons"))))

;; Writing

(defn create
  "A writer for a new GeoPackage at `file`, which must not exist yet (or
  be empty). Add things to it with `add-network!` and
  `add-map-features!`, and then call `finish!`."
  [file]
  {:file    (.getCanonicalFile (clojure.java.io/as-file file))
   :tables  (atom {})
   :network (atom 0)})

(defn- connection ^java.sql.Connection [w]
  (jdbc/get-connection (str "jdbc:sqlite:" (:file w))))

(defn- quote-id [s] (str \" (string/replace s "\"" "\"\"") \"))

(defn- unique-labels
  "`columns`, with a number after the :label of any whose label an
  earlier one has (a user field can be called ID, say)"
  [columns]
  (second
   (reduce
    (fn [[seen out] {:keys [label] :as c}]
      (let [label (->> (cons label (for [i (iterate inc 2)] (str label " " i)))
                       (remove seen)
                       (first))]
        [(conj seen label) (conj out (assoc c :label label))]))
    [#{} []]
    columns)))

(defn- sqlite-type [type]
  (case type :real "REAL" :integer "INTEGER" :boolean "BOOLEAN" "TEXT"))

(defn- write-rows!
  "Write `rows` into `table`, creating it, or adding any of `columns` it
  hasn't got yet.

  `columns` are maps with a :label (the human name, which identifies the
  column in the table), a :key to get the value from a row, a :type,
  and maybe a :description. A column the table already has keeps its
  type. `rows` can be large and lazy, as they are written as they are
  read. If `geometry` is given, it's a map with :type and :key, for
  the geometry of a feature table."
  [w table {:keys [description geometry]} columns rows]
  (let [columns (unique-labels columns)
        known (get @(:tables w) table)
        exists (boolean known)

        ;; name new columns, and use the type of existing ones
        state
        (reduce
         (fn [state {:keys [label type]}]
           (if (get-in state [:columns label])
             state
             (let [column (unique-identifier label (:used state))]
               (-> state
                   (assoc-in [:columns label] {:column column :type type :new true})
                   (update :used conj column)))))
         (or known {:columns {} :used reserved-columns
                    :description description :spatial (boolean geometry)})
         columns)

        columns (for [c columns] (merge c (get-in state [:columns (:label c)])))

        schema
        (concat
         (if geometry
           [["geom" {:type (:type geometry) :srid srid :accessor (:key geometry)}]]
           ;; an integer primary key makes it an attributes table in gpkg_contents
           [["fid" {:type :integer :primary-key :auto-increment :accessor (constantly nil)}]])
         (for [{:keys [column type key label description]} columns]
           [column (cond-> {:type type
                            :accessor (comp (partial sql-value type) key)
                            :name label
                            :title label}
                     description (assoc :description description))]))]

    (when exists
      (when-let [new-columns (seq (filter :new columns))]
        (with-open [c (connection w)]
          (doseq [{:keys [column type]} new-columns]
            (jdbc/execute! c [(format "ALTER TABLE %s ADD COLUMN %s %s"
                                      (quote-id table) (quote-id column) (sqlite-type type))])))))

    (gpkg/write (:file w) table rows
                :schema schema
                :if-exists :append
                :add-spatial-index (boolean geometry))

    (swap! (:tables w) assoc table
           (update state :columns update-vals #(dissoc % :new)))))

(defn- write-table!
  "Write a table, whose `columns` are like a spreadsheet's (with :name,
  :key and maybe :format and :description), and whose `rows` are all in
  memory. The columns' types are worked out from their values."
  [w table options columns rows]
  (let [columns (vec columns)
        keys    (mapv safe-key columns)
        geometry-key (-> options :geometry :key)
        rows    (vec
                 (for [row rows
                       :let [values (mapv #(% row) keys)]
                       ;; the spreadsheet has blank rows to space things out
                       :when (or geometry-key
                                 (some some? (map (fn [c v] (when-not (::constant c) v))
                                                  columns values)))]
                   (cond-> values geometry-key (conj (geometry-key row)))))
        n       (count columns)]
    (write-rows!
     w table
     (cond-> options geometry-key (assoc-in [:geometry :key] #(nth % n)))
     (for [[i c] (map-indexed vector columns)]
       {:label (:name c)
        :description (:description c)
        :type  (infer-type (:format c) (map #(nth % i) rows))
        :key   #(nth % i)})
     rows)))

(defn- network-columns
  "The columns at the start of each table which say which `network` (its
  :id in the GeoPackage, and :name) a row is for"
  [network]
  [{:name "Network ID" :key (constantly (:id network)) :format :integer ::constant true
    :description "Which network this is about: see the networks table"}
   {:name "Network" :key (constantly (:name network)) :format :text ::constant true
    :description "The name of the network"}])

(defn- write-candidate-layers!
  "Write `candidates` as features in `table`, or in tables named after it
  for those whose geometry isn't of dimension `usual` (see `layer-name`).
  `geometry` gets a candidate's geometry."
  [w network table usual description columns candidates & {:keys [geometry]
                                                               :or {geometry candidate-geometry}}]
  (let [candidates (for [c candidates] [c (geometry c)])
        layers     (group-by (fn [[_ g]] (layer-name table usual (dimension g))) candidates)
        columns    (concat (network-columns network) columns)]
    ;; the usual layer is always there, so the layers are the same for
    ;; each network; others are only there if they're needed
    (doseq [[layer candidates] (merge {table []} layers)
            :let [dim (or (some-> candidates first second dimension) usual)]]
      (write-table!
       w layer
       {:description description
        :geometry {:type (multi-types dim) :key (fn [[_ g]] (some-> g ->multi))}}
       (for [c columns] (update c :key #(comp % first)))
       candidates))))

(defn- unique-by-name [columns]
  (->> columns
       (reduce (fn [[seen out] c]
                 (if (seen (:name c)) [seen out] [(conj seen (:name c)) (conj out c)]))
               [#{} []])
       (second)))

(defn- building-columns
  "The building layer's columns, which are those of all the spreadsheet's
  building sheets"
  [doc]
  (if (document/has-solution? doc)
    (unique-by-name
     (concat (network-model/networked-building-columns doc)
             (network-model/other-building-columns doc)))
    (network-model/building-columns doc)))

(defn- format-time [t]
  (cond
    (instance? java.util.Date t) (str (.toInstant ^java.util.Date t))
    (some? t) (str t)))

(defn- write-networks-row! [w network {:keys [id created author map-id map-name]} doc]
  (write-table!
   w "networks"
   {:description "The networks in this GeoPackage, whose network_id is in each table"}
   (concat
    (network-columns network)
    [{:name "THERMOS ID" :key (constantly id) :format :integer
      :description "The network's id in THERMOS, if it's saved, which is in its URL"}
     {:name "Saved" :key (constantly (format-time created)) :format :text
      :description "When this version of the network was saved (UTC)"}
     {:name "Author" :key (constantly author) :format :text}
     {:name "Map ID" :key (constantly map-id) :format :integer}
     {:name "Map" :key (constantly map-name) :format :text}
     {:name "Mode" :key (constantly (name (document/mode doc)))
      :description "heating or cooling"}
     {:name "Objective" :key (constantly (some-> (::document/objective doc :network) name))
      :description "network (the network's NPV) or system (the whole system's NPV)"}
     {:name "Has solution" :key (constantly (document/has-solution? doc)) :format :boolean}])
   [doc]))

(defn encode-document
  "`doc` as it goes in network_documents: EDN, gzipped, in base64 (as
  geometry.gpkg doesn't write BLOBs). They're big, and only for THERMOS."
  [doc]
  (let [out (java.io.ByteArrayOutputStream.)]
    (with-open [w (-> (java.util.zip.GZIPOutputStream. out)
                      (java.io.OutputStreamWriter. "UTF-8"))]
      (binding [*print-length* nil *print-level* nil *print-namespace-maps* false]
        (print-method doc w)))
    (.encodeToString (java.util.Base64/getEncoder) (.toByteArray out))))

(defn decode-document
  "The EDN text of a document from `encode-document`"
  ^String [^String s]
  (with-open [in (-> (.decode (java.util.Base64/getDecoder) s)
                     (java.io.ByteArrayInputStream.)
                     (java.util.zip.GZIPInputStream.))]
    (String. (.readAllBytes in) "UTF-8")))

(defn- write-document! [w network doc]
  (write-table!
   w "network_documents"
   {:description "Each network's THERMOS document, so it can be uploaded into THERMOS again"}
   (concat
    (network-columns network)
    [{:name "Format version" :key (constantly piecewise/current-version) :format :integer
      :description "The version of THERMOS's document format the document has"}
     {:name "Document" :key (constantly (encode-document doc)) :format :text
      :description "The document, as gzipped EDN in base64"}])
   [doc]))

(defn- write-solver! [w network doc objective-trace]
  (let [[b1 b2] (::solution/bounds doc)]
    (write-table!
     w "solver"
     {:description "How the optimiser ran, for each network which has been run"}
     (concat
      (network-columns network)
      [{:name "State" :key (comp #(some-> % name) ::solution/state)
        :description "valid, optimal, infeasible, error..."}
       {:name "Message" :key ::solution/message :format :text}
       {:name "Runtime (s)" :key ::solution/runtime :format :double}
       {:name "Objective" :key ::solution/objective :format :double
        :description "The objective value of the solution"}
       {:name "Gap" :key ::solution/gap :format :double
        :description "The optimiser's relative gap at the end, if it finished with an exact solve"}
       {:name "Lower bound" :key (constantly (when (and b1 b2) (min b1 b2))) :format :double}
       {:name "Upper bound" :key (constantly (when (and b1 b2) (max b1 b2))) :format :double}
       {:name "Iterations" :key ::solution/iterations :format :integer
        :description "How many solutions were evaluated while fixing parameters; see solver_iterations"}
       {:name "Method" :key (comp #(when (seq %) (string/join ", " (map name %))) ::solution/stages)
        :format :text
        :description "The methods the optimiser used, in order"}
       {:name "Stopped early" :key (comp boolean ::solution/stopped-early) :format :boolean
        :description "Whether the run was stopped before its time limit, keeping its best solution"}])
     (when doc [doc]))

    (write-table!
     w "solver_iterations"
     {:description "The objective value after each iteration of fixing parameters"}
     (concat
      (network-columns network)
      [{:name "Iteration" :key first :format :integer}
       {:name "Objective" :key second :format :double}])
     (map-indexed (fn [i o] [(inc i) o]) (::solution/objectives doc)))

    (write-table!
     w "objective_trace"
     {:description "The objective of the best solution so far, as the optimiser ran"}
     (concat
      (network-columns network)
      [{:name "Time (s)" :key first :format :double
        :description "Seconds since the optimiser started"}
       {:name "Objective" :key second :format :double}])
     objective-trace)))

(defn- remove-network!
  "Delete the rows for network `network-id` from every table which has
  them, so a network which failed part way through leaves nothing
  behind. The tables (and any columns it added) stay."
  [w network-id]
  (let [column (identifier (:name (first (network-columns nil))))]
    (with-open [c (connection w)]
      (doseq [{:keys [table_name]}
              (jdbc/execute! c ["SELECT table_name FROM gpkg_contents"]
                             {:builder-fn rs/as-unqualified-maps})
              :when (some #(= column (:name %))
                          (jdbc/execute! c [(str "PRAGMA table_info(" (quote-id table_name) ")")]
                                         {:builder-fn rs/as-unqualified-maps}))]
        (jdbc/execute! c [(format "DELETE FROM %s WHERE %s = ?"
                                  (quote-id table_name) (quote-id column))
                          network-id])))))

(declare write-network!)

(defn add-network!
  "Add the network document `doc` to the GeoPackage being written by `w`.

  `info` describes it, for the networks table: its :id, :name, :created
  time, :author, :map-id and :map-name, any of which can be
  missing. `objective-trace` is the [seconds objective] pairs of how the
  optimiser improved its solution, if known (from the job's progress
  data). Returns the network_id it has in the GeoPackage.

  If it fails, whatever of the network was written is removed before
  the exception is thrown, so the caller can carry on without it."
  [w info doc & {:keys [objective-trace]}]
  (let [network-id (swap! (:network w) inc)]
    (try
      (write-network! w network-id info doc objective-trace)
      (catch Exception e
        (when (.exists ^java.io.File (:file w))
          (try (remove-network! w network-id)
               (catch Exception e2
                 (log/error e2 "Removing network" network-id "from GeoPackage"))))
        (throw e)))))

(defn- write-network! [w network-id info doc objective-trace]
  (let [network {:id network-id :name (or (:name info) (str "Network " network-id))}
        {buildings :building paths :path} (document/candidates-by-type doc)]
    (write-networks-row! w network info doc)
    (write-document! w network doc)

    (write-candidate-layers!
     w network "buildings" 2
     "Buildings in each network, with their solution if it has one"
     (building-columns doc)
     buildings)

    (write-candidate-layers!
     w network "paths" 1
     "Paths in each network, with their pipes if it has a solution"
     (network-model/path-columns doc)
     paths)

    (write-candidate-layers!
     w network "supply_points" 0
     "The buildings which can be supplies, as points within them"
     (network-model/supply-point-columns doc)
     (network-model/supply-points doc)
     :geometry #(some-> (candidate-geometry %) (.getInteriorPoint)))

    (doseq [{tab :name columns :columns rows :rows} (spreadsheet/to-tables doc)]
      (write-table!
       w (identifier tab)
       {:description (str "The " tab " sheet of the THERMOS spreadsheet")}
       (concat (network-columns network) columns)
       rows))

    (write-solver! w network (when (document/has-solution? doc) doc) objective-trace)
    network-id))

;; The map's own data

(defn- user-field-columns
  "Columns for user fields, from `user-fields`, a map of field name to
  :real, :boolean or :text"
  [user-fields]
  (for [[field type] (sort-by key user-fields)]
    {:label field :type type
     :key #(get (:user-fields %) field)}))

(def ^:private map-building-columns
  [{:label "ID" :type :text :key :geoid
    :description "The candidate's id in THERMOS, which is the ID column of the network layers"}
   {:label "Original ID" :type :text :key :orig-id
    :description "The feature's id in the GIS data it came from"}
   {:label "Annual demand (kWh)" :type :real :key :demand-kwh-per-year}
   {:label "Peak demand (kW)" :type :real :key :demand-kwp}
   {:label "Demand model" :type :text :key :demand-source
    :description "How the demand was estimated"}
   {:label "Peak model" :type :text :key :peak-source
    :description "How the peak demand was estimated"}
   {:label "Annual cooling demand (kWh)" :type :real :key :cooling-kwh-per-year}
   {:label "Peak cooling demand (kW)" :type :real :key :cooling-kwp}
   {:label "Connections" :type :integer :key :connection-count
    :description "How many addresses the building has"}
   {:label "Height (m)" :type :real :key :height}
   {:label "Floor area (m²)" :type :real :key :floor-area}
   {:label "Wall area (m²)" :type :real :key :wall-area}
   {:label "Roof area (m²)" :type :real :key :roof-area}
   {:label "Ground area (m²)" :type :real :key :ground-area}
   {:label "Connection group" :type :integer :key :conn-group
    :description "Buildings in the same group were joined to the paths together"}
   {:label "Connected to" :type :text :key #(some->> (:connection-id %) seq (string/join ","))
    :description "The ids of the paths the building is connected to"}])

(def ^:private map-path-columns
  [{:label "ID" :type :text :key :geoid
    :description "The candidate's id in THERMOS, which is the ID column of the network layers"}
   {:label "Original ID" :type :text :key :orig-id
    :description "The feature's id in the GIS data it came from"}
   {:label "Start" :type :text :key :start-id :description "The id of the junction at the start"}
   {:label "End" :type :text :key :end-id :description "The id of the junction at the end"}
   {:label "Length (m)" :type :real :key :length}])

(def map-columns
  "The key of the rows from thermos-backend.db.maps each column of
  map_buildings and map_paths is from, by its label; the other columns
  are user fields. Connected to is a list, so it isn't here. For
  reading them back, see thermos-backend.gpkg.upload."
  (into {}
        (for [{:keys [label key]} (concat map-building-columns map-path-columns)
              :when (keyword? key)]
          [label key])))

(defn- read-once
  "An Iterable over the seq `s` which can be read once, and doesn't
  hold onto its head, so that a long lazy `s` needn't all be in memory"
  [s]
  (let [s (volatile! s)]
    (reify Iterable
      (iterator [_]
        (reify java.util.Iterator
          (hasNext [_] (some? (vswap! s seq)))
          (next [_] (let [x (first @s)] (vswap! s rest) x)))))))

(defn add-map-info!
  "Add the map table, about the map called `map-name`, whose import
  `parameters` (from the new map wizard) are kept so it can be uploaded
  into THERMOS again"
  [w map-name parameters]
  (write-table!
   w "map"
   {:description "The map whose buildings and paths are in map_buildings and map_paths"}
   [{:name "Name" :key (constantly map-name) :format :text}
    {:name "Parameters" :key (constantly (pr-str parameters)) :format :text
     :description "How the map was made in THERMOS, as EDN"}]
   [nil]))

(defn add-map-features!
  "Add a map's candidates of `kind` (:buildings or :paths) whose geometry
  has dimension `dim` to map_buildings or map_paths (or a layer named
  after them, see `layer-name`).

  `rows` are the candidates, as from thermos-backend.db.maps, with the
  :geometry as geojson. They can be lazy, and many, as they are written
  as they're read. `user-fields` is a map of their user fields' names
  to type (:real, :boolean or :text), for the columns."
  [w kind dim rows user-fields]
  (let [[table usual columns description]
        (case kind
          :buildings ["map_buildings" 2 map-building-columns
                      "All the buildings on the map, which networks are made from"]
          :paths     ["map_paths" 1 map-path-columns
                      "All the paths on the map, which networks are made from"])
        [id-column & columns] columns]
    (write-rows!
     w (layer-name table usual dim)
     {:description description
      :geometry {:type (multi-types dim)
                 :key #(some-> (:geometry %) ->geometry ->multi)}}
     (concat [id-column] (user-field-columns user-fields) columns)
     (read-once rows))))

(defn finish!
  "Finish writing the GeoPackage of `w`: set each feature table's extent
  and description, and take it out of WAL mode, so it's all in one file."
  [w]
  (with-open [c (connection w)]
    (doseq [[table {:keys [spatial description]}] @(:tables w)]
      (when description
        (jdbc/execute! c ["UPDATE gpkg_contents SET description = ? WHERE table_name = ?"
                          description table]))
      (when spatial
        ;; gpkg/write sets the extent of what it last wrote, but there may
        ;; have been several networks
        (let [[{:keys [minx miny maxx maxy]}]
              (jdbc/execute! c [(format "SELECT min(minx) minx, min(miny) miny, max(maxx) maxx, max(maxy) maxy FROM %s"
                                        (quote-id (str "rtree_" table "_geom")))]
                             {:builder-fn rs/as-unqualified-lower-maps})]
          (when minx
            (jdbc/execute! c ["UPDATE gpkg_contents SET min_x = ?, min_y = ?, max_x = ?, max_y = ? WHERE table_name = ?"
                              minx miny maxx maxy table])))))
    (jdbc/execute! c ["PRAGMA journal_mode = DELETE"]))
  (:file w))

(defn write-file!
  "Write a GeoPackage into `file` by calling `f` with a writer, and then
  `finish!`. If that fails, the file is deleted."
  [file f]
  (try
    (doto (create file)
      (f)
      (finish!))
    (catch Exception e
      (.delete (clojure.java.io/as-file file))
      (throw e)))
  file)

(ns thermos-cli.gpkg-export
  (:require
   [clojure.string :as string]
   [clojure.java.io :as io]
   [clojure.java.jdbc :as jdbc])
  (:import [org.locationtech.jts.geom Geometry Envelope]
           [org.geotools.geopkg GeoPackage FeatureEntry]
           [org.geotools.data DataUtilities DefaultTransaction]
           [org.geotools.referencing CRS] 
           [org.geotools.geometry.jts Geometries ReferencedEnvelope]
           [org.geotools.jdbc JDBCFeatureReader$ResultSetFeature]))


(defn- sqlite-query! [file query-params]
  (let [db-spec {:dbtype "sqlite"
                 :dbname (.getCanonicalPath (io/as-file file))}]
    (jdbc/execute! db-spec query-params)))

(defn- ->crs [x]
  (cond (string? x) (CRS/decode x true)
        (integer? x) (CRS/decode (str "EPSG:" x) true)
        :else (throw (IllegalArgumentException. (str "Unknown type of CRS " x)))))


(defprotocol HasGeometry
  (geometry ^org.locationtech.jts.geom.Geometry [g])
  (update-geometry [x g]))


(extend-type Geometry HasGeometry
             (geometry [g] g)
             (update-geometry [_ g] g))


(defrecord Feature [^Geometry geometry table crs]
  HasGeometry
  (geometry [f] (:geometry f))
  (update-geometry [f g] (assoc f :geometry g)))

(defn srid [g] (.getSRID (geometry g)))

(defn point? [g] (and (satisfies? HasGeometry g)
                      (= "Point" (.getGeometryType (geometry g)))))



(defn- kv-type [k v]
  [(name k)
   (cond->
    {:accessor #(get % k)
     :type
     (if (instance? Geometry v)
       Geometries/GEOMETRY
       (case (.getCanonicalName (class v))
         ("java.lang.String"
          "java.lang.Float"
          "java.lang.float"
          "java.lang.Double"
          "java.lang.double"
          "java.lang.Boolean"
          "java.lang.boolean"
          "java.lang.Short"
          "java.lang.short"
          "java.lang.Integer"
          "java.lang.int"
          "java.lang.Long"
          "java.util.Date"
          "java.sql.Date"
          "java.sql.Time"
          "java.sql.Timestamp"
          "java.math.BigDecimal") (.getCanonicalName (class v))
         (str name ":String")))}
     (instance? Geometry v)
     (assoc :srid (srid v)))])

(defn- infer-spec
  "Given a feature, get a spec for its fields inferring types from their values.

  This function should probably only be used interactively or for hacking around,
  as it is not very safe if you might have nil values (which have no type)"

  [feature]
  (cond (instance? Feature feature)
        (let [keys (keys feature)]
          (concat
           [["geometry"
             {:type Geometries/GEOMETRY
              :srid  (srid feature)
              :accessor geometry}]]
           (for [k (sort-by str keys)
                 :when (and (not= k :geometry)
                            (not= k :table)
                            (not= k :crs))]
             (kv-type k (get feature k)))))

        (satisfies? HasGeometry feature)
        [["geometry"
          {:type Geometries/GEOMETRY
           :accessor geometry
           :srid (srid feature)}]]

        (map? feature)
        (for [[k v] (sort-by (comp str first) feature)] (kv-type k v))

        :else
        (throw (ex-info "Unable to infer schema for value" {:value feature}))))


(defn- open-for-writing
  "Open a gpkg for writing spatial data via the geotools APIs.

   Also sets the batch insert size (via reflection)."
  ^GeoPackage [file batch-insert-size]
  (let [geopackage (GeoPackage. (io/as-file file))]
    (.init geopackage)
    (let [m (.getDeclaredMethod GeoPackage "dataStore" nil)]
      (.setAccessible m true)
      (let [ds (.invoke m geopackage nil)]
        (.setBatchInsertSize ds batch-insert-size)))
    geopackage))

(defn- spec-geom-field
  "Get the spec for the (first) geometry field from a schema."
  [spec]
  (first (filter
          (fn [[_ {:keys [type]}]]
            (or
             (= Geometries/GEOMETRY type)
             (= Geometries/GEOMETRYCOLLECTION type)
             (= Geometries/LINESTRING type)
             (= Geometries/MULTILINESTRING type)
             (= Geometries/MULTIPOINT type)
             (= Geometries/MULTIPOLYGON type)
             (= Geometries/POINT type)
             (= Geometries/POLYGON type)

             (= :geometry type)
             (= :geometry-collection type)
             (= :line-string type)
             (= :multi-line-string type)
             (= :multi-point type)
             (= :multi-polygon type)
             (= :point type)
             (= :polygon type)))
          spec)))

(defn- ->geotools-type
  "Convert a type specification from a schema into something that geotools can understand
   as a column type."
  [type]
  (cond
    (class? type)  (.getCanonicalName ^Class type)
    (string? type) type

    (= :geometry type) Geometries/GEOMETRY
    (= :geometry-collection type) Geometries/GEOMETRYCOLLECTION
    (= :line-string type) Geometries/LINESTRING
    (= :multi-line-string type) Geometries/MULTILINESTRING
    (= :multi-point type) Geometries/MULTIPOINT
    (= :multi-polygon type) Geometries/MULTIPOLYGON
    (= :point type) Geometries/POINT
    (= :polygon type) Geometries/POLYGON

    (= :integer type) (.getCanonicalName Integer)
    (= :int type) (.getCanonicalName Integer)
    (= :long type) (.getCanonicalName Long)
    (= :short type) (.getCanonicalName Short)
    (= :float type) (.getCanonicalName Float)
    (= :double type) (.getCanonicalName Double)
    (= :real type) (.getCanonicalName Double)
    (= :string type) (.getCanonicalName String)
    (= :boolean type) (.getCanonicalName Boolean)

    (keyword? type) (.getCanonicalName String)

    :else type))

(defn ->feature-entry [table-name spec srid]
  (let [geom-col (spec-geom-field spec)]
    (doto (FeatureEntry.)
      (.setTableName table-name)
      (.setGeometryColumn (first geom-col))
      (.setGeometryType (->geotools-type (:type (second geom-col))))
      (.setBounds (ReferencedEnvelope. 0 0 0 0 (CRS/decode (str "EPSG:" srid)))))))


(defn- set-layer-extent!
  "Update the extent (if not nil) for `table-name` in the geopackage at `file`

  return `file` in case you want to thread"
  [file table-name  ^ReferencedEnvelope layer-extent]
  (when layer-extent
    (sqlite-query!
     file
     ["UPDATE gpkg_contents SET min_x = ?, min_y = ?, max_x = ?, max_y = ? WHERE table_name = ?;"
      (.getMinX layer-extent)
      (.getMinY layer-extent)
      (.getMaxX layer-extent)
      (.getMaxY layer-extent)
      table-name]))
  file)


(defn- ->geotools-schema
  "Create a geotools schema that defines the columns of a gpkg table."
  [table-name spec]
  (DataUtilities/createType
   table-name
   (string/join ","
                (for [[name {:keys [type srid]}] spec]
                  (let [type (->geotools-type type)]
                    (str name ":" type (when srid (str ":srid=" srid))))))))


(defn write
  "Write the given sequence of `features` into a geopackage at `file`
  in a table called `table-name`

  The `schema` argument is good to supply; without it the schema will be
  inferred which may not be what you want.

  A schema looks like a series of tuples, like

  [field-name field-attributes]

  field-attributes is a map having :type, :accessor and optionally :srid
  for geometry types.

  :type is the canonical name of a java class, or just :String, or Geometries/GEOMETRY.

  Look at `infer-spec` for examples.

   Returns nil.
  "
  ([file table-name ^Iterable features & {:keys [schema batch-insert-size]
                                          :or {batch-insert-size 4000}}]
   {:pre [(or (instance? Iterable features) (nil? features))]}
   (with-open [geopackage (open-for-writing file batch-insert-size)]
     (let [spec (vec (or schema (infer-spec (first features))))
           [geom-field {:keys [srid]
                        :or   {srid 27700}}] (spec-geom-field spec)
           crs (CRS/decode (str "EPSG:" srid))
           emit-feature (let [getters
                              (vec (for [[k v] spec]
                                     (or (:accessor v)
                                         #(get % k))))]
                          (fn [feature]
                            (mapv #(% feature) getters)))
           feature-entry ^FeatureEntry (->feature-entry table-name spec srid)]
       (.setBounds feature-entry (ReferencedEnvelope. 0 0 0 0 crs))
       (try
         (.create geopackage feature-entry (->geotools-schema table-name spec))
         (catch java.lang.IllegalArgumentException _))

       (try (.createSpatialIndex geopackage feature-entry)
            (catch java.io.IOException e
              (println "Spatial index already exists for table")))

       (let [features (or features [])
             iter ^java.util.Iterator (.iterator ^java.lang.Iterable features)
             ^ReferencedEnvelope layer-extent
             (with-open [tx (DefaultTransaction.)]
               (let [extent
                     (with-open [writer (.writer geopackage feature-entry true nil tx)]
                       (loop [^ReferencedEnvelope extent nil]
                         (if (.hasNext iter)
                           (let [feature (.next iter)
                                 writable-feature (.next writer)]
                             ;; this is here because there is a problem with underlying geotools 
                             ;; whereby the fid is pulled in from userData (an empty HashMap) - see:
                             ;; https://github.com/geotools/geotools/blob/dbc12274458ccfb1963240e86782f0ed976cae29/modules/library/jdbc/src/main/java/org/geotools/jdbc/JDBCInsertFeatureWriter.java#L130
                             ;; a non-nil string placeholder value is needed to complete the transaction else 
                             ;; there will be a NullPointerException (fid must not be null)
                             (let [user-data (.getUserData writable-feature)]
                               (.put user-data "fid" ""))
                             (.setAttributes
                              ^JDBCFeatureReader$ResultSetFeature writable-feature
                              ^java.util.List (emit-feature feature))
                             (.write writer)
                             (recur
                              (let [^Geometry geom (or (get feature geom-field)
                                                       (get feature (keyword geom-field))
                                                       (get feature (name geom-field)))
                                    ^Envelope feature-env
                                    (cond
                                      (nil? geom) nil
                                      (point? geom) (Envelope. (.getCoordinate geom))
                                      :else (Envelope. (.getEnvelopeInternal geom)))]
                                (cond
                                  (nil? feature-env) extent
                                  (nil? extent) (ReferencedEnvelope. feature-env crs)
                                  :else (doto extent (.expandToInclude feature-env))))))
                           extent)))]
                 (.commit tx)
                 extent))]
         (set-layer-extent! file table-name layer-extent))))
   nil))


(comment
  (def test-schema
    [[:geom {:type Geometries/POINT :srid 4326 :accessor :geom}]
     [:name {:type java.lang.String :accessor :name}]
     [:population {:type java.lang.Integer :accessor :population}]
     [:area {:type java.lang.Double :accessor :area}]
     [:capital? {:type java.lang.Boolean :accessor :capital?}]])


  (def test-features
    [{:geom (.createPoint (org.locationtech.jts.geom.GeometryFactory.)
                          (org.locationtech.jts.geom.Coordinate. 0.1278 51.5074)) ; London
      :name "London"
      :population 9000000
      :area 1572.0
      :capital? true}

     {:geom (.createPoint (org.locationtech.jts.geom.GeometryFactory.)
                          (org.locationtech.jts.geom.Coordinate. 2.3522 48.8566)) ; Paris
      :name "Paris"
      :population 2148000
      :area 105.4
      :capital? true
      :fid 1}

     {:geom (.createPoint (org.locationtech.jts.geom.GeometryFactory.)
                          (org.locationtech.jts.geom.Coordinate. 13.4050 52.5200)) ; Berlin
      :name "Berlin"
      :population 3769000
      :area 891.8
      :capital? true}])


  (def test-schema
    [[:geom {:type org.locationtech.jts.geom.Point :srid 4326 :accessor :geom}]
     [:name {:type java.lang.String :accessor :name}]
     [:population {:type java.lang.Integer :accessor :population}]
     [:area {:type java.lang.Double :accessor :area}]
     [:capital? {:type java.lang.Boolean :accessor :capital?}]])

  (def test-features
    [{:geom (doto (.createPoint (org.locationtech.jts.geom.GeometryFactory.)
                                (org.locationtech.jts.geom.Coordinate. 0.1278 51.5074))
              (.setSRID 4326))
      :name "London"
      :population 9000000
      :area 1572.0
      :capital? true}

     {:geom (doto (.createPoint (org.locationtech.jts.geom.GeometryFactory.)
                                (org.locationtech.jts.geom.Coordinate. 2.3522 48.8566))
              (.setSRID 4326))
      :name "Paris"
      :population 2148000
      :area 105.4
      :capital? true}

     {:geom (doto (.createPoint (org.locationtech.jts.geom.GeometryFactory.)
                                (org.locationtech.jts.geom.Coordinate. 13.4050 52.5200))
              (.setSRID 4326))
      :name "Berlin"
      :population 3769000
      :area 891.8
      :capital? true}])
  (write "test4.gpkg" "cities" test-features :keys test-schema))

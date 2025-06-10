(ns thermos-cli.gpkg-export
  (:require
   [clojure.string :as string]
   [clojure.java.io :as io])
  (:import [org.locationtech.jts.geom Geometry Envelope]
           [org.geotools.geopkg GeoPackage FeatureEntry]
           [org.geotools.data DataUtilities DefaultTransaction DataStoreFinder]
           [org.geotools.referencing CRS]
           [org.geotools.data.simple SimpleFeatureStore]
           [org.geotools.feature.simple SimpleFeatureBuilder SimpleFeatureTypeBuilder]
           [org.geotools.feature DefaultFeatureCollection]
           [org.geotools.geometry.jts Geometries ReferencedEnvelope]
           [org.geotools.jdbc JDBCFeatureReader$ResultSetFeature]))



;; start fresh
;; start with producing 

;; FeatureEntry needs to be generated
;; see here: https://docs.geotools.org/stable/javadocs/org/geotools/geopkg/FeatureEntry.html
;; This alters the following:

"CREATE TABLE gpkg_contents (
  table_name TEXT NOT NULL PRIMARY KEY,
  data_type TEXT NOT NULL,
  identifier TEXT UNIQUE,
  description TEXT DEFAULT '',
  min_x DOUBLE,
  min_y DOUBLE,
  max_x DOUBLE,
  max_y DOUBLE,
  srs_id INTEGER
);"

;; and

"CREATE TABLE gpkg_geometry_columns (
  table_name TEXT NOT NULL,
  column_name TEXT NOT NULL,
  geometry_type_name TEXT NOT NULL,
  srs_id INTEGER NOT NULL
);"

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
  "Refactor of set-layer-extent to use GeoTools api"
  [^GeoPackage geopackage ^FeatureEntry feature-entry ^ReferencedEnvelope layer-extent]
  (when layer-extent
    (.setBounds feature-entry layer-extent)
    (.update geopackage feature-entry))
  geopackage)


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
  ([file table-name ^Iterable features & {:keys [schema batch-insert-size add-spatial-index]
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

       (when add-spatial-index
         (try (.createSpatialIndex geopackage feature-entry)
              (catch java.io.IOException e
                (println e "Unable to create spatial index in %s on %s" file table-name))))
       
       (let [features (or features [])
             iter ^java.util.Iterator (.iterator ^java.lang.Iterable features) 

             ^ReferencedEnvelope layer-extent
             (with-open [tx (DefaultTransaction.)]
               (let [extent
                     (with-open [writer (.writer geopackage feature-entry true nil tx)]
                       (loop [^ReferencedEnvelope extent nil]
                         (if (.hasNext iter)
                           (let [feature (.next iter)
                                 values (emit-feature feature)
                                 writable-feature (.next writer)]
                             (println writable-feature (class writable-feature))
                             ;; this is here because there is a problem with underlying geotools 
                             ;; whereby the fid is pulled in from userData (an empty HashMap) - see:
                             ;; https://github.com/geotools/geotools/blob/dbc12274458ccfb1963240e86782f0ed976cae29/modules/library/jdbc/src/main/java/org/geotools/jdbc/JDBCInsertFeatureWriter.java#L130
                             ;; a non-nil string placeholder value is needed to complete the transaction else 
                             ;; there will be a NullPointerException
                             (let [user-data (.getUserData writable-feature)]
                               (.put user-data
                                     "fid"
                                     ""))

                             (dotimes [i (count values)]
                               (.setAttribute writable-feature i (nth values i)))
                             (.write writer)
                             ;;  (.setAttributes
                             ;;   ^JDBCFeatureReader$ResultSetFeature (.next writer)
                             ;;   ^java.util.List (emit-feature feature))
                             ;;  (.write writer)
                             (recur
                              (let [^Geometry geom (get feature geom-field)
                                    ^Envelope feature-env
                                    (cond
                                      (nil? geom) nil
                                      (point? geom) (Envelope. (.getCoordinate geom))
                                      :else (Envelope. (.getEnvelopeInternal geom)))]
                                (cond
                                  (nil? feature-env) extent

                                  (nil? extent) (ReferencedEnvelope. feature-env crs)

                                  :else (doto extent (.expandToInclude feature-env))))))
                           extent)))] ;; return extent
                 (println "333")
                 (.commit tx)
                 extent))] ;; and return extent

         (set-layer-extent! file table-name layer-extent)
         )))
   nil))


;; I couldn't get the implementation as shown in clj-geometry
;; I kept encountering this fid null pointer error when attempting to commit the transaction
;; Execution error (NullPointerException) at org.geotools.filter.identity.FeatureIdImpl/setID (FeatureIdImpl.java:55).
;; fid must not be null
;; this method below is a little clunkier, utilising the SimpleFeatureTypeBuilder over FeatureEntry
;; However, one issue is that it doesn't update layer extent in system tables

(defn build-feature-type
  "Given a table name, schema and crs, this creates a spatial table
   in a geopackage"
  [^String table-name spec crs]
  (let [builder (doto (SimpleFeatureTypeBuilder.)
                  (.setName table-name)
                  (.setCRS crs))]
    (doseq [[field-name {:keys [type]}] spec]
      (println field-name)
      (.add builder (name field-name) type))
    (.buildFeatureType builder)))

(defn write-gpkg [file table-name features schema srid]
  (let [params {"dbtype" "geopkg" "database" file}
        datastore (DataStoreFinder/getDataStore params)]
    (try
      (let [crs (CRS/decode (str "EPSG:" srid))
            feature-type (build-feature-type table-name schema crs)]

        ;; this clause checks if the table exists in the datastore, if not 
        ;; then create it using feature-type schema
        (when-not (some #{table-name} (.getTypeNames datastore))
          (.createSchema datastore feature-type))

        ;; 
        (let [^SimpleFeatureStore store (.getFeatureSource datastore table-name)
              tx (DefaultTransaction. "create")
              builder (SimpleFeatureBuilder. feature-type)
              collection (DefaultFeatureCollection. nil feature-type)]

          (try
            (doseq [feature features]
              (.reset builder)
              (doseq [[k {:keys [accessor]}] schema]
                (let [val ((or accessor #(get feature k)) feature)]
                  (.add builder val)))
              (.add collection (.buildFeature builder nil)))
            (.setTransaction store tx)
            (.addFeatures store collection)
            (.commit tx)

            (catch Exception e
              (.rollback tx)
              (throw e))
            (finally
              (.close tx)))))
      (finally
        ;; THIS IS NEEDED TO CLOSE GPKG correctly, else:
        ;; SEVERE: There's code using JDBC based datastore and not disposing them. This may lead to temporary loss of database connections. Please make sure all data access code calls DataStore.dispose() before freeing all references to it
        (.dispose datastore)))))


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

(ns thermos-backend.maps.db
  (:require [jdbc.core :as jdbc]
            [honeysql.core :as sql]
            [honeysql.helpers :refer :all]
            [honeysql.format :as fmt]
            [honeysql-postgres.format :refer :all]
            [honeysql-postgres.helpers :refer :all]
            
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [thermos-backend.db :as db]
            ))

(declare find-polygon find-tile make-bounding-points comma-separate space-separate)

(def ^:dynamic *insert-block-size* 1000)

(defn insert!
  "GEOJSON-FILE should be the path to a geojson file containing
  information about some candidates."
  [db geojson-file progress]
  (log/info "Inserting candidates from" geojson-file)
  (db/with-connection [conn db]
    (let [geojson-data (-> (io/file geojson-file)
                           (slurp)
                           (json/read-str :key-fn keyword))

          crs (get-in geojson-data [:crs :properties :name] "EPSG:4326")
          epsg (if (.startsWith crs "EPSG:")
                 (Integer/parseInt (.substring crs 5))
                 (do
                   (log/warn "Unknown CRS" crs "- using 4326!")
                   4326))
          
          features (:features geojson-data)

          _ (log/debug (count features) "features to insert")
          
          sql-geometry
          (fn [geom]
            (sql/call :ST_SetSRID (sql/call :ST_GeomFromGeoJSON (json/write-str geom)) epsg))

          features (group-by (comp :id :properties) features)

          features (for [[_ dups] features]
                     (if (> (count dups) 1)
                       (do
                         (log/warn "Duplicate features" (map :properties dups))
                         (first dups))
                       (first dups)))
          ]


      (doseq [[type features] (group-by (comp :type :properties) features)]
        (progress [:put type (count features)])
        (doseq [block (partition-all *insert-block-size* features)]
          (try
            (jdbc/atomic
             conn
             (-> (insert-into :candidates)
                 (values (for [{properties :properties
                                geometry :geometry} block]
                           {:id (:id properties)
                            :orig_id (or (:orig_id properties) "unknown")
                            :name (or (:name properties) "")
                            :type (or (:subtype properties) "")
                            :geometry (sql-geometry geometry)}))
                 (upsert (-> on-conflict :id)
                         (do-update-set :orig_id :name :type :geometry))
                 (sql/format)
                 (->> (jdbc/execute conn)))
             (-> (case type
                   (":polygon" "building")
                   (-> (insert-into :buildings)
                       (values (for [{properties :properties} block]
                                 {:id (:id properties)
                                  :connection_id (sql/call :regexp_split_to_array
                                                           (:connection_id properties)
                                                           ",")
                                  :demand_kwh_per_year (:demand_kwh_per_year properties)
                                  :demand_kwp (:demand_kwp properties)
                                  ;; TODO real connection count
                                  :connection_count 1}))
                       (upsert (-> (on-conflict :id)
                                   (do-update-set :connection_id :demand_kwh_per_year :demand_kwp
                                                  :connection_count)))
                       )
                   (":linestring" "path")
                   (-> (insert-into :paths)
                       (values (for [{properties :properties} block]
                                 {:id (:id properties)
                                  :start_id (:start_id properties)
                                  :end_id (:end_id properties)
                                  :length (:length properties)
                                  :unit_cost (or (:unit_cost properties) 0)
                                  }))
                       (upsert (-> (on-conflict :id)
                                   (do-update-set :start_id :end_id :length :unit_cost))))

                   (log/warn "Unknown type of feature" type))
                 (sql/format)
                 (->> (jdbc/execute conn))))
            
            (progress [:done type (count block)])
            (catch Exception e
              (log/error e "Inserting a block of" type)
              (progress e)))))
      
      )))

(defn find-tile [db zoom x-tile y-tile]
  (->> (make-bounding-points zoom x-tile y-tile)
       (find-polygon db)))

(defn find-polygon [db points]
  (let [query
        (-> (select :id :name :type :geometry :is_building
                    :demand_kwh_per_year :demand_kwp :connection_count :connection_ids
                    :start_id :end_id :length :unit_cost)
            (from :joined_candidates) ;; this view is defined in the migration SQL
            (where [:&& :raw_geometry (sql/call :ST_GeomFromText (sql/param :box) (int 4326))]))

        box-string
        (format "POLYGON((%s))" (comma-separate (map space-separate points)))

        tidy-fields
        #(into {} (filter second %)) ;; keep only map entries with non-nil values
        ]
    (db/with-connection [conn db]
      (-> query
          (sql/format {:box box-string})
          (->> (jdbc/fetch conn)
               (map tidy-fields))))))

(def comma-separate (partial string/join ","))
(def space-separate (partial string/join " "))

(defn- make-bounding-points [zoom x-tile y-tile]
  (let [n (Math/pow 2 zoom)
        to-lon-lat (fn [x y n]
                     (let [lon-deg (- (* (/ x n) 360) 180)
                           lat-rad (Math/atan
                                    (Math/sinh
                                     (* (Math/PI)
                                        (- 1
                                           (* 2 (/ y n))))))
                           lat-deg (Math/toDegrees lat-rad)]
                       [lon-deg lat-deg]))]
    [(to-lon-lat x-tile y-tile n)
     (to-lon-lat x-tile (+ 1 y-tile) n)
     (to-lon-lat (+ 1 x-tile) (+ 1 y-tile) n)
     (to-lon-lat (+ 1 x-tile) y-tile n)
     (to-lon-lat x-tile y-tile n)
     ]))

(defmethod fmt/fn-handler "&&" [_ a b & more]
  (if (seq more)
    (apply fmt/expand-binary-ops "&&" a b more)
    (str (fmt/to-sql-value a) " && " (fmt/to-sql-value b))))

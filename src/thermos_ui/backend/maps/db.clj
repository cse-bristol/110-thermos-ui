(ns thermos-ui.backend.maps.db
  (:require [clojure.java.jdbc :as j]
            [ragtime.jdbc]
            [ragtime.repl]
            [honeysql.core :as sql]
            [honeysql.helpers :refer :all]
            [honeysql.format :as fmt]
            [honeysql-postgres.format :refer :all]
            [honeysql-postgres.helpers :refer :all]
            [thermos-ui.backend.config :refer [config]]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            ))

;; Access to the database containing buildings and ways

(def database
  (future-call
   #(let [db-config {:dbtype   "postgresql"
                     :dbname   (config :pg-db-geometries)
                     :host     (config :pg-host)
                     :user     (config :pg-user)
                     :password (config :pg-password)}
          ragtime-config {:datastore (ragtime.jdbc/sql-database db-config)
                          :migrations (ragtime.jdbc/load-resources "migrations")}
          ]

      (ragtime.repl/migrate ragtime-config)
      db-config)))

(declare find-polygon find-tile make-bounding-points comma-separate space-separate)

(defn insert!
  "GEOJSON-FILE should be the path to a geojson file containing
  information about some candidates. The candidates need to have a
  type on them for this to work."
  [geojson-file progress]
  (log/info "Inserting candidates from" geojson-file)
  (let [features (-> (io/file geojson-file)
                     (slurp)
                     (json/read-str :key-fn keyword)
                     :features)

        _ (log/info (count features) "candidates read from file")
        
        sql-geometry
        (fn [geom]
          (sql/call
           :ST_SetSRID
           (sql/call :ST_GeomFromGeoJSON (json/write-str geom))
           (int 4326)))

        get-properties
        (fn [feature defaults]
          (merge defaults (select-keys (:properties feature) (conj (keys defaults) :id))))

        candidate-defaults
        {:name "" :type "" :subtype ""}

        building-defaults
        {:connection_id "" :demand 0}

        path-defaults
        {:start_id "" :end_id ""}

        insert-common-data
        (fn [candidates]
          (-> (insert-into :candidates)
              (values (for [candidate candidates]
                        (clojure.core/update
                         (merge (get-properties candidate candidate-defaults)
                                {:geometry (sql-geometry (:geometry candidate))})
                         :type
                         #(sql/call :candidate_type %)
                         )))
              (upsert (-> (on-conflict :id)
                          (do-update-set :name :subtype :geometry)))
              ))

        insert-other-data
        (fn [type candidates]
          (case (keyword type)
            (:supply :demand)
            (-> (insert-into :buildings)
                (values (for [candidate candidates]
                          (clojure.core/update
                           (get-properties candidate building-defaults)
                           :connection_id
                           #(sql/call :regexp_split_to_array % ",")
                           )))
                (upsert (-> (on-conflict :id)
                            (do-update-set :connection_id))))
            :path
            (-> (insert-into :paths)
                (values (for [candidate candidates]
                          (get-properties candidate path-defaults)))
                (upsert (-> (on-conflict :id)
                            (do-update-set :start_id :end_id)))
                )))

        distinct-id
        (fn [candidates]
          (map (comp first second)
               (group-by (comp :id :properties) candidates)))
        ]
    (j/with-db-transaction [tx @database]
      (doseq [[type features]  (group-by (comp :type :properties) features)]
        (let [features (distinct-id features)]
          (progress [:put type (count features)])
          (doseq [block (partition-all 1000 features)]
            ;; We can insert all the candidate data in one go, but the
            ;; building / path data is split and has to go in
            ;; separately. Fortunately, because we are generating the
            ;; keys outside the db we don't have to do anything to keep
            ;; the keys consistent
            (try
              (do
                (log/info "Inserting a block of" type)
                (j/execute! tx (sql/format (insert-common-data block)))
                (j/execute! tx (sql/format (insert-other-data type block)))
                (progress [:done type (count block)]))
              (catch Exception e
                (println e)
                (progress e))))
          )))))

(defn find-tile [zoom x-tile y-tile]
  (-> (make-bounding-points zoom x-tile y-tile)
      (find-polygon)))

(defn find-polygon [points]
  (let [query
        (-> (select :id :name :type :subtype :connection_id
                    :demand :start_id :end_id
                    :geometry :simple_geometry)

            (from :joined_candidates) ;; this view is defined in the migration SQL
            (where [:&& :real_geometry (sql/call :ST_GeomFromText (sql/param :box) (int 4326))]))

        box-string
        (format "POLYGON((%s))" (comma-separate (map space-separate points)))

        tidy-fields
        #(into {} (filter second %)) ;; keep only map entries with non-nil values
        ]
    (->> (j/query @database (sql/format query {:box box-string}))
         (map tidy-fields)
         )
    ))

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

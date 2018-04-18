(ns thermos-ui.backend.maps.db
  (:require [clojure.java.jdbc :as j]
            [ragtime.jdbc]
            [ragtime.repl]
            [honeysql.core :as sql]
            [honeysql.helpers :refer :all]
            [honeysql.format :as fmt]
            [thermos-ui.backend.config :refer [config]]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.string :as string]
            ))

;; Access to the database containing buildings and ways

(def database
  {:dbtype   "postgresql"
   :dbname   (config :pg-db-geometries)
   :host     (config :pg-host)
   :user     (config :pg-user)
   :password (config :pg-password)})

(def ragtime-config
  {:datastore (ragtime.jdbc/sql-database database)
   :migrations (ragtime.jdbc/load-resources "migrations")}
  )

(ragtime.repl/migrate ragtime-config)

(declare find-polygon find-tile make-bounding-points comma-separate space-separate)

(def building-fields
  [:id :address :postcode :type :building_type :demand :connect_id
   [(sql/call :ST_AsGeoJSON :geometry) :geometry]])

(def building-defaults
  {:id ""
   :address ""
   :postcode ""
   :type "demand"
   :building_type ""
   :demand 0
   :connect_id ""})

(def way-fields
  [:id :address :postcode :length :start_id :end_id
   [(sql/call :ST_AsGeoJSON :geometry) :geometry]])

(def way-defaults
  {:id "" :address "" :postcode "" :length 0 :start_id "" :end_id ""})

(defn find-tile [zoom x-tile y-tile]
  (let [bounding-points (make-bounding-points zoom x-tile y-tile)
        buildings (find-polygon bounding-points :buildings building-fields)
        ways (map
              #(assoc % :type "path")
              (find-polygon bounding-points :ways way-fields))
        result (concat buildings ways)
        ]
    (println zoom x-tile y-tile (count result))
    result
    ))

(defn insert!
  "BUILDINGS-DATA should be the path to a geojson file containing
  information about some buildings"
  [geojson-file table defaults]

  (let [features (-> (io/file geojson-file)
                     (slurp)
                     (json/read-str :key-fn keyword)
                     :features)

        clean-feature
        (fn [{geometry :geometry
              properties :properties}]
          (merge
           defaults

           (select-keys properties (keys defaults))

           {:geometry
            (sql/call
             :ST_SetSRID
             (sql/call :ST_GeomFromGeoJSON (json/write-str geometry))
             (int 4326)
             )}))
        ]
    ;; seems we can't insert too many things at once, so we do them in blocks of a thousand
    (j/with-db-transaction [tx database]
      (doseq [block (partition-all 1000 (map clean-feature features))]
        (j/execute! tx (sql/format (-> (insert-into table)
                                       (values block)))))
      true)))

(defn insert-buildings!
  [geojson-file]
  (insert! geojson-file :buildings building-defaults))

(defn insert-ways!
  [geojson-file]
  (insert! geojson-file :ways way-defaults))

(defn find-polygon [points table fields]
  (let [query
        (-> (apply select fields)
            (from table)
            (where [:&& :geometry (sql/call :ST_SetSRID
                                            (sql/call :ST_GeomFromText (sql/param :box))
                                            (int 4326))]))

        box-string
        (format "POLYGON((%s))" (comma-separate (map space-separate points)))
        ]

    (j/query database (sql/format query {:box box-string}))
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

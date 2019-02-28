(ns thermos-backend.db.maps
  (:require [thermos-backend.db :as db]
            [thermos-backend.db.st-functions :as st]
            [honeysql.helpers :as h]
            [honeysql-postgres.helpers :as p]
            [honeysql-postgres.format]
            [jdbc.core :as jdbc]
            [honeysql.core :as sql]
            [honeysql.types :as sql-types]
            [honeysql.format :as fmt]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [thermos-backend.maps.heat-density :as heat-density]))

(defmethod fmt/fn-handler "&&" [_ a b & more]
  (if (seq more)
    (apply fmt/expand-binary-ops "&&" a b more)
    (str (fmt/to-sql-value a) " && " (fmt/to-sql-value b))))

(defn create-map!
  "Create a new map within the project."
  [project-id map-name description]
  {:pre [(int? project-id)
         (string? map-name)
         (string? description)
         (not (string/blank? map-name))]}

  (db/insert-one!
   :maps {:project-id project-id :name map-name}))

(def ^:dynamic *insert-block-size* 1000)

(defn- geom-from-text
  ([geom]
   (st/geomfromtext geom))
  
  ([geom srid]
   (st/geomfromtext geom (sql/inline (int srid)))))

(def candidates-keys
  [:map-id :id :orig-id :name :type :geometry])

(def buildings-keys
  [:map-id :id :connection-id :demand-kwh-per-year :demand-kwp :connection-count])

(def paths-keys
  [:map-id :id :start-id :end-id :length :unit-cost])

(defn insert-into-map!
  "Insert data into a map, possibly erasing what is already there.
  Later we will want to amend this to stitch together old map and new map."
  [& {:keys [erase-geometry
             srid
             format
             buildings
             paths
             map-id]
      :or {srid 4326 format :wkt}}]
  {:pre [(integer? map-id)
         (= :wkt format)]}
  
  (db/with-connection [conn]
    (let [deleted
          (-> (h/delete-from :candidates)
              (h/where [:&& :geometry (geom-from-text erase-geometry srid)])
              (db/execute! conn))]

      ;; TODO this should delete all affected density tiles as well
      (log/info "Deleted" deleted "candidates in target area"))

    (doseq [chunk (partition-all *insert-block-size* buildings)]
      (jdbc/atomic
       conn
       (-> (h/insert-into :candidates)
           (h/values (for [c chunk]
                       (-> c
                           (select-keys candidates-keys)
                           (update :geometry #(geom-from-text % srid))
                           (assoc :map-id map-id))))
           (db/execute! conn))

       (-> (h/insert-into :buildings)
           (h/values (for [b chunk]
                       (-> b
                           (select-keys buildings-keys)
                           (update :connection-id
                                   #(sql-types/array
                                     (string/split % #","))))))
           (db/execute! conn))))

    (doseq [chunk (partition-all *insert-block-size* paths)]
      (jdbc/atomic
       conn
       (-> (h/insert-into :candidates)
           (h/values (for [c chunk]
                       (-> c
                           (select-keys candidates-keys)
                           (update :geometry #(geom-from-text % srid))
                           (assoc :map-id map-id))))
           (db/execute! conn))

       (-> (h/insert-into :paths)
           (h/values (for [p chunk]
                       (-> p
                           (select-keys paths-keys))))
           (db/execute! conn))))

    ;; (re)generate map icon and so on
    (-> (h/select (sql/call :update_map (int map-id)))
        (db/fetch! conn))
    ))

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


(def comma-separate (partial string/join ","))
(def space-separate (partial string/join " "))
(defn polygon-wkt [points]
  (format "POLYGON((%s))" (comma-separate (map space-separate points))))

(defn get-polygon [map-id points]
  (let [query
        (-> (h/select :id :name :type :geometry :is_building
                      :demand_kwh_per_year :demand_kwp :connection_count :connection_ids
                      :start_id :end_id :length :unit_cost)
            (h/from :joined_candidates) ;; this view is defined in the migration SQL
            (h/where [:and
                      [:= :map-id map-id]
                      [:&& :raw_geometry
                       (st/geomfromtext
                        (sql/param :box)
                        (sql/inline (int 4326)))]]))

        box-string (polygon-wkt points)
        tidy-fields
        #(into {} (filter second %)) ;; keep only map entries with non-nil values

        query (sql/format query {:box box-string})
        ]
    (map tidy-fields
         (db/fetch! query))))

(defn get-tile
  "Query out the vector data from the given webmercator tile
  Returns geojson featurecollection type thing"
  [map-id zoom x-tile y-tile]
  (let [features
        (->> (make-bounding-points zoom x-tile y-tile)
             (get-polygon map-id))]
    {:type :FeatureCollection
     :features (for [f features]
                 {:type :Feature
                  :geometry (:geometry f)
                  :properties (dissoc f :geometry)})}))

(defn- get-density-points [map-id [tl-x tl-y] [br-x br-y] bw]
  (-> (h/select :x :y :demand)
      (h/from :heat-centroids)
      (h/where [:and
                [:= :map-id map-id]
                [:&& :geometry
                 (as-> (polygon-wkt
                        [[tl-x tl-y]     ;; Top-left x         o--|
                         [br-x tl-y]     ;; Top-right y              |__|
                         [br-x br-y]     ;; Bottom-right y    |__o
                         [tl-x br-y]     ;; Bottom-left y           o__|
                         [tl-x tl-y]     ;; Top-left y again   |__|
                         ]) box
                   
                   (st/geogfromtext box)
                   (st/buffer box bw)
                   (st/geometry box))]])
      (db/fetch!)))

(defn- get-cached-density-tile [conn map-id z x y]
  (-> (h/select :bytes)
      (h/from :tilecache)
      (h/where [:and
                [:= :map-id map-id]
                [:= :z z]
                [:= :x x]
                [:= :y y]])
      (db/fetch-one! conn)
      (:bytes)))

(defn- get-density-maximum [conn map-id z]
  (-> (h/select :maximum)
      (h/from :tilemaxima)
      (h/where [:and
                [:= :map-id map-id]
                [:= :z z]])
      (db/fetch-one! conn)
      (:maximum)))

(defn get-density-tile [map-id z x y]
  (db/with-connection [conn]
    (let [existing-tile    (get-cached-density-tile conn map-id z x y)
          existing-maximum (or (get-density-maximum conn map-id z) 0.01)]
      (if existing-tile
        (heat-density/colour-float-matrix existing-tile existing-maximum)

        (let [[new-maximum new-tile]
              (heat-density/density-image :x x :y y :z z :bandwidth 30 :size 256
                                          :get-values
                                          (fn [tl br bwm]
                                            (get-density-points map-id tl br bwm)))
              
              new-maximum (max new-maximum (or existing-maximum 0))]
          (-> (h/insert-into :tilecache)
              (h/values [{:bytes new-tile :x x :y y :z z :map-id map-id}])
              (db/execute!))

          (when (> new-maximum existing-maximum)
            (-> (h/insert-into :tilemaxima)
                (h/values [{:z z :map-id map-id :maximum new-maximum}])
                (p/upsert (-> (p/on-conflict :z :map-id)
                              (p/do-update-set! [:maximum
                                                 (sql/call :greatest
                                                           :EXCLUDED.maximum
                                                           :tilemaxima.maximum)])))
                (db/execute! conn)))

          (heat-density/colour-float-matrix new-tile new-maximum))))))

(defn get-map-centre
  "Get the lat/lon coordinates for the centre of the map"
  [map-id]
  (-> (h/select :*)
      (h/from :map-centres)
      (h/where [:= :map-id map-id])
      (db/fetch-one!)))

(defn get-icon
  "Try and render an icon for the map"
  [map-id]
  (-> (h/select :png)
      (h/from :map-icons)
      (h/where [:= :map-id map-id])
      (db/fetch-one!)
      (:png)))

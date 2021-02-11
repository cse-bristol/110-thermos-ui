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
            [clojure.edn :as edn]
            [clojure.tools.logging :as log]
            [thermos-backend.maps.heat-density :as heat-density]
            [clojure.data.json :as json]
            [thermos-util :refer [distinct-by] :as util]))

(defmethod fmt/fn-handler "&&" [_ a b & more]
  (if (seq more)
    (apply fmt/expand-binary-ops "&&" a b more)
    (str (fmt/to-sql-value a) " && " (fmt/to-sql-value b))))

(defn create-map!
  "Create a new map within the project."
  [project-id map-name description parameters]
  {:pre [(int? project-id)
         (string? map-name)
         (string? description)
         (not (string/blank? map-name))]}

  (db/insert-one!
   :maps {:project-id project-id :name map-name
          :parameters (pr-str parameters)}))

(defn set-job-id! [map-id job-id]
  {:pre [(int? map-id)
         (int? job-id)]}
  (-> (h/update :maps)
      (h/sset {:job-id job-id})
      (h/where [:= :id map-id])
      (db/execute!)))

(defn get-map
  "Get the detail of this map.
  Returns at least keys :project-id :name :parameters."
  [map-id]
  {:pre [(int? map-id)]}
  (-> (h/select :*)
      (h/from :maps)
      (h/where [:= :id map-id])
      (db/fetch!)
      (first)
      (update :parameters edn/read-string)))

(def ^:dynamic *insert-block-size* 1000)

(defn- geom-from-text
  ([geom]
   (st/geomfromtext geom))
  
  ([geom srid]
   (st/geomfromtext geom (sql/inline (int srid)))))

(def candidates-keys
  [:map-id :geoid :orig-id :user-fields :geometry])

(def buildings-keys
  [:candidate-id :connection-id :demand-kwh-per-year
   :demand-kwp :connection-count :demand-source :peak-source
   :floor-area :height :wall-area :roof-area :ground-area
   :cooling-kwh-per-year
   :cooling-kwp
   :conn-group])

(def paths-keys
  [:candidate-id :start-id :end-id :length])

(defn- data-values
  "A helper to create a vector to in a WITH statement for honeysql that creates a temporary VALUES table.

  `alias` is the name of the temporary table, and `values` are the values, as you might pass to honeysql.helpers/values.

  If you write (helpers/with (data-values :some-data [...])) then you can join/select etc from :some-data in the rest of the query.

  Unfortunately if all the values in a block are null the type will be inferred as string
  "
  [alias values]
  (let [all-keys (sort (reduce into #{} (for [v values] (keys v))))
        all-values
        (fn [value]
          `("(" ~@(interpose
                   ", "
                   (map
                    ;; unfortunately some of these values are strings
                    #(let [v (get value %)]
                       (if (string? v)
                         ;; if we return a string into the list of values,
                         ;; it will be interpolated as a raw string into the output
                         ;; so we do a horrible thing here to prevent this.
                         (sql/call :text v)
                         v)
                       )
                    all-keys)) ")"))]
    [(sql/raw
      `[~(sql/quote-identifier alias)
        "("
        ~@(flatten (interpose ", " (for [k all-keys] (sql/quote-identifier k))))
        ")"])
     (sql/raw
      `["(VALUES "
        ~@(flatten (interpose ", " (map all-values values)))
        ")"])]))

(defn- to-jsonb [x] (sql-types/call :cast (json/write-str x) :jsonb))

(defn- clean-user-fields
  "Filter map `m` to take out any values that are nil, stringify all
  keys, and make sure values are expressible as simple json values.
  This will convert any nested structures in user-fields to strings
  I'm afraid."
  [m]
  (persistent!
   (reduce-kv
    (fn [a k v]
      (cond-> a (not (or (nil? v) (and (string? v) (string/blank? v))))
              (assoc! (str k)
                      (cond
                        (or (boolean? v) (number? v)) v
                        (string? v)
                        (or (util/as-double v) (util/as-boolean v) v)
                        :else (str v))
                      )))
    (transient {}) m)))

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
  
  (let [tag (str "map import " map-id ":")
        buildings (distinct-by buildings :geoid)
        paths     (distinct-by paths :geoid)]
    (db/with-connection [conn]
      (let [deleted
            (-> (h/delete-from :candidates)
                (h/where [:and
                          [:&& :geometry (geom-from-text erase-geometry srid)]
                          [:= :map-id map-id]])
                (db/execute! conn))]

        ;; TODO this should delete all affected density tiles as well
        (log/info tag "deleted" deleted "candidates in target area"))

      (doseq [block (partition-all *insert-block-size* buildings)]
        (jdbc/atomic
            conn

          ;; This weird syntax is the way to insert related rows in pgsql
          ;; 
          ;; What we're doing is of the form
          ;; WITH tablename (cols) AS (VALUES ()...),
          ;;      inserts AS (INSERT INTO x SELECT FROM tablename),
          ;; INSERT INTO other (select stuff from tablename join inserts)
          ;;
          ;; so we can get hold of the new FK from inserts for use in other
          ;; TODO if two candidates have the same geoid then we get a pkey collision
          
          (-> (h/with
               (data-values :building-data
                            (for [c block]
                              (-> c
                                  (select-keys
                                   (remove #{:candidate-id}
                                           (concat candidates-keys buildings-keys)))
                                  
                                  (update :connection-id
                                          #(sql-types/array
                                            (string/split % #",")))

                                  ;; need to do this in case all null
                                  (update :conn-group
                                          #(sql-types/call :cast % :integer))

                                  (update :user-fields (comp to-jsonb clean-user-fields))
                                  
                                  (assoc :map-id map-id)
                                  (update :geometry #(geom-from-text % srid)))))

               [:new-candidates
                (-> (h/insert-into [[:candidates candidates-keys]
                                    (-> (apply h/select candidates-keys)
                                        (h/from :building-data))])
                    (p/returning :geoid [:id :candidate-id]))])

              (h/insert-into [[:buildings buildings-keys]
                              (-> (apply h/select buildings-keys)
                                  (h/from :new-candidates)
                                  (h/join :building-data
                                          [:= :new-candidates.geoid :building-data.geoid]))])

              (db/execute! conn))))

      (doseq [block (partition-all *insert-block-size* paths)]
        (jdbc/atomic
            conn
          ;; this ought to work???
          (-> (h/with
               (data-values :path-data
                            (for [c block]
                              (-> c
                                  (select-keys (remove #{:candidate-id}
                                                       (concat candidates-keys paths-keys)))
                                  (assoc :map-id map-id)
                                  (update :user-fields (comp to-jsonb clean-user-fields))
                                  
                                  (update :geometry #(geom-from-text % srid)))))

               [:new-candidates
                (-> (h/insert-into [[:candidates candidates-keys]
                                    (-> (apply h/select candidates-keys)
                                        (h/from :path-data))])
                    (p/returning :geoid [:id :candidate-id]))])

              (h/insert-into [[:paths paths-keys]
                              (-> (apply h/select paths-keys)
                                  (h/from :new-candidates)
                                  (h/join :path-data
                                          [:= :new-candidates.geoid :path-data.geoid]))])

              (db/execute! conn)
              )))

      (log/info tag "inserted" (+ (count paths) (count buildings)) "candidates")
      ;; (re)generate map icon and so on
      ;; this sometimes dies, so we will retry thrice
      (loop [n 3]
        (when (pos? n)
          (recur (try
                   (-> (h/select (sql/call :update_map (int map-id)))
                       (db/fetch! conn))
                   0
                   (catch Exception e (dec n))))))

      (log/info tag "updated summary information")
      
      (-> (h/update :maps)
          (h/sset {:import-completed true})
          (h/where [:= :id map-id])
          (db/execute! conn))
      (log/info tag "import finished"))))

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
        (-> (h/select :id :user_fields :geometry :is_building
                      :demand_kwh_per_year
                      :cooling_kwh_per_year
                      :cooling_kwp
                      :demand_source
                      :demand_kwp :connection_count :connection_ids
                      :conn_group
                      :start_id :end_id :length
                      :floor_area :height :wall_area :roof_area :ground_area)
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
         (binding [db/*json-key-fn* identity] (db/fetch! query)))))

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

(defn- get-density-points [map-id [tl-x tl-y] [br-x br-y] bw is-heat]
  (-> (h/select :x :y (if is-heat :demand [:cold-demand :demand]))
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

(defn- get-cached-density-tile [conn map-id z x y is-heat]
  (-> (h/select :bytes)
      (h/from :tilecache)
      (h/where [:and
                [:= :map-id map-id]
                [:= :z z]
                [:= :x x]
                [:= :y y]
                [:= :is-heat (boolean is-heat)]])
      (db/fetch-one! conn)
      (:bytes)))

(defn- get-density-maximum [conn map-id z is-heat]
  (-> (h/select :maximum)
      (h/from :tilemaxima)
      (h/where [:and
                [:= :map-id map-id]
                [:= :z z]
                [:= :is-heat (boolean is-heat)]])
      (db/fetch-one! conn)
      (:maximum)))

(defn get-density-tile [map-id z x y is-heat]
  (db/with-connection [conn]
    (let [existing-tile    (get-cached-density-tile conn map-id z x y is-heat)
          existing-maximum (or (get-density-maximum conn map-id z is-heat) 0.01)]
      (if existing-tile
        (heat-density/colour-float-matrix existing-tile existing-maximum is-heat)

        (let [[new-maximum new-tile]
              (heat-density/density-image :x x :y y :z z :bandwidth 30 :size 256
                                          :get-values
                                          (fn [tl br bwm]
                                            (get-density-points map-id tl br bwm is-heat)))
              
              new-maximum (max new-maximum (or existing-maximum 0))]
          (-> (h/insert-into :tilecache)
              (h/values [{:bytes new-tile :x x :y y :z z :map-id map-id
                          :is-heat (boolean is-heat)}])
              (db/execute!))

          (when (> new-maximum existing-maximum)
            (-> (h/insert-into :tilemaxima)
                (h/values [{:z z :map-id map-id :maximum new-maximum
                            :is-heat (boolean is-heat)}])
                (p/upsert (-> (p/on-conflict :z :map-id :is-heat)
                              (p/do-update-set! [:maximum
                                                 (sql/call :greatest
                                                           :EXCLUDED.maximum
                                                           :tilemaxima.maximum)])))
                (db/execute! conn)))

          (heat-density/colour-float-matrix new-tile new-maximum is-heat))))))


(defn get-map-centre
  "Get the lat/lon coordinates for the centre of the map"
  [map-id]
  (-> (h/select
       :map-id
       [(st/x :envelope) :x]
       [(st/y :envelope) :y])
      (h/from :map-centres)
      (h/where [:= :map-id map-id])
      (db/fetch-one!)))

(defn get-map-bounds
  "Get the map boundingbox"
  ([map-id]
   (-> (h/select
        :map-id
        [(st/xmin :envelope) :x-min]
        [(st/xmax :envelope) :x-max]
        [(st/ymin :envelope) :y-min]
        [(st/ymax :envelope) :y-max])
       (h/from :map-centres)
       (h/where [:= :map-id map-id])
       (db/fetch-one!))))

(defn get-map-bounds-as-geojson []
  (let [bounds
        (-> (h/select
             :map-centres.map-id :maps.name
             [(sql/call :json (st/asgeojson :envelope)) :envelope])
            (h/left-join :maps [:= :maps.id :map-centres.map-id])
            (h/from :map-centres)
            (db/fetch!))
        ]
    {:type :FeatureCollection
     :features
     (for [bound bounds]
       {:type :Feature
        :geometry (:envelope bound)
        :id (:map-id bound)
        :properties {:id (:map-id bound)
                     :name (:name bound)}})}))

(defn get-icon
  "Try and render an icon for the map"
  [map-id]
  (-> (h/select :png)
      (h/from :map-icons)
      (h/where [:= :map-id map-id])
      (db/fetch-one!)
      (:png)))

(defn delete-map! [map-id]
  (-> (h/delete-from :maps)
      (h/where [:= :id map-id])
      (db/execute!)))

(defn stream-features [map-id callback]
  (binding [db/*json-key-fn* identity]
    (db/with-connection [conn]
      (jdbc/atomic conn
        (with-open [cursor
                    (-> (h/select :candidates.geoid :orig-id :user-fields
                                  :connection-id
                                  :demand-kwh-per-year
                                  :demand-source
                                  :demand-kwp
                                  :cooling-kwh-per-year
                                  :cooling-kwp
                                  :connection-count
                                  [(sql/call :ST_AsGeoJson
                                             :geometry) :geometry])
                        (h/from :candidates)
                        (h/join :buildings [:= :candidates.id :buildings.candidate-id])
                        (h/where [:= :map-id map-id])
                        (sql/format)
                        (->> (jdbc/fetch-lazy conn)))]
          (doseq [row (jdbc/cursor->lazyseq cursor)]
            (callback row)))
        
        (with-open [cursor
                    (-> (h/select :candidates.geoid :orig-id :user-fields
                                  :start-id :end-id :length
                                  [(sql/call :ST_AsGeoJson
                                             :geometry) :geometry])
                        (h/from :candidates)
                        (h/join :paths [:= :candidates.id :paths.candidate-id])
                        (h/where [:= :map-id map-id])
                        (sql/format)
                        (->> (jdbc/fetch-lazy conn)))]
          (doseq [row (jdbc/cursor->lazyseq cursor)]
            (callback row)))
        ))))


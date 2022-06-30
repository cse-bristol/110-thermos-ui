;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.importer.process
  (:require [thermos-backend.util :as util]
            [thermos-backend.config :refer [config]]

            [clojure.set :as set]
            [clojure.java.io :as io]
            [org.tobereplaced.nio.file :as nio]

            [thermos-importer.geoio :as geoio]
            [thermos-importer.overpass :as overpass]
            [thermos-importer.lidar :as lidar]
            [thermos-importer.svm-predict :as svm]
            [thermos-importer.lm-predict :as lm]
            [thermos-importer.spatial :as topo]
            [thermos-importer.util :refer [has-extension file-extension]]
            [thermos-backend.importer.sap :as sap]
            [thermos-backend.importer.cooling :as cooling]
            [thermos-backend.project-lidar :as project-lidar]

            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.data.csv :as csv]
            [thermos-importer.spatial :as spatial]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [thermos-util :refer [as-double as-boolean assoc-by distinct-by annual-kwh->kw]]
            [clojure.pprint :refer [pprint]]
            [cljts.core :as jts]
            [clojure.test :as test])
  
  (:import [org.locationtech.jts.geom
            Envelope
            GeometryFactory
            PrecisionModel]))

(defn- keyword-upcase-uscore [s]
  (keyword (str/replace s "_" "-")))

(defn- clean-tag [t]
  (and t (str/capitalize (str/replace t #"[_-]+" " "))))

(defn- remap-building-type
  "We did have a complicated program which took stuff from a file, but
  this is what it boils down to:

  1. If the building has an amenity tag, use that
  2. If it has a non-yes building tag, use that
  3. If it has a landuse (added by overpass.clj) use that"
  [b]
  (let [landuse  (:landuse b)
        building (:building b)
        amenity  (:amenity b)
        building (when-not (= "yes" building) building)]
    (clean-tag (or amenity building landuse))))

(defn- geojson-bounds [{{[coordinates] "coordinates"} "geometry"}]
  (let [lons (map first coordinates)
        lats (map second coordinates)

        min-lon (reduce min lons)
        max-lon (reduce max lons)
        min-lat (reduce min lats)
        max-lat (reduce max lats)

        ;; TODO this probably fails around the antimeridian, and maybe
        ;; to its left more generally
        ]
    [min-lat min-lon max-lat max-lon]))

(let [osm-lock (Object.)]
  (defn- query-osm [state parameters]
    (locking osm-lock
      (let [get-buildings (-> parameters :geometry :source (= :osm))
            get-roads     (or (-> parameters :roads :include-osm)
                              get-buildings)

            query-area    (if get-buildings
                            (or
                             (-> parameters :geometry :osm :osm-id)
                             ;; this needs way: or rel: on it

                             ;; bounding box query
                             (-> parameters :geometry :osm :boundary geojson-bounds))

                            ;; we are only getting the roads, so we want
                            ;; to get bounds from the existing geometry

                            (let [box (geoio/bounding-box (:buildings state))]
                              [(.getMinY box)
                               (.getMinX box)
                               
                               (.getMaxY box)
                               (.getMaxX box)]))

            set-building-type  #(assoc % :subtype (remap-building-type %))
            set-road-type      #(assoc % :subtype (clean-tag (:highway %)))
            set-osm-height     #(let [osm-height (as-double (:height %))
                                      osm-levels (as-double (:building:levels %))
                                      osm-height (and (number? osm-height)
                                                      (pos? osm-height)
                                                      osm-height)
                                      osm-levels (and (number? osm-levels)
                                                      (pos? osm-levels)
                                                      osm-levels)]
                                  (cond-> (dissoc % :height)
                                    osm-levels
                                    (assoc :storeys osm-levels)

                                    osm-height
                                    (assoc :fallback-height osm-height)))
            
            set-specials       #(cond-> %
                                  (= (:osm-id %) "289662492")
                                  (assoc :name "Rothballer Towers")
                                  (= (:osm-id %) "602252862")
                                  (assoc :name "The Thumimdome"))
            
            query-results (overpass/get-geometry query-area
                                                 :include-buildings get-buildings
                                                 :include-highways get-roads)

            query-results
            (for [r query-results]
              (assoc r :identity (:osm-id r)))
            
            buildings
            (->> query-results
                 (filter :building)
                 (geoio/explode-multis)
                 (filter (comp #{:polygon} ::geoio/type))
                 (map set-building-type)
                 (map set-specials)
                 (map set-osm-height))

            highways
            (->> query-results
                 (filter :highway)
                 ;; some highways are polygons!
                 (keep
                  (fn [i]
                    (cond
                      (= :line-string (::geoio/type i)) i
                      (= :polygon (::geoio/type i))
                      ;; If it's a polygon, we want to turn it into a line string.
                      ;; We can do this by replacing it with its exterior ring.
                      ;; This is safe, since we're going to node it later.
                      (let [geometry (::geoio/geometry i)]
                        (when (zero? (.getNumInteriorRing geometry))
                          (geoio/update-geometry
                           i
                           (.getExteriorRing geometry)))))))
                 (map set-road-type))

            ;; the overpass query handler puts in :name and :subtype
            ;; which we want to transfer into [:user-fields "Name"] and
            ;; [:user-fields "Classification"]
            copy-to-udf
            (fn [features udfs]
              (for [feature features]
                (reduce-kv
                 (fn [feature k v]
                   (assoc-in feature [:user-fields v] (get feature k)))
                 feature udfs)))

            buildings (copy-to-udf buildings {:name "Name" :subtype "Category"})
            highways  (copy-to-udf highways {:name "Name" :subtype "Category"})
            ]

        (cond-> state
          get-buildings
          (assoc :buildings
                 {::geoio/crs "EPSG:4326" ::geoio/features buildings})

          get-roads
          (update :roads
                  (fn [user-roads]
                    {::geoio/crs "EPSG:4326"
                     ::geoio/features
                     (if user-roads
                       (let [crs (::geoio/crs user-roads)
                             feat (::geoio/features user-roads)
                             rp (geoio/reprojector crs "EPSG:4326")]
                         (concat highways (map rp feat)))
                       highways)})))
        ))))

(defn load-lidar-index [project-id]
  (when-let [lidar-directory (config :lidar-directory)]
      (->> (file-seq (io/file lidar-directory))
           (filter #(project-lidar/can-access-tiff? project-id %))
           (filter project-lidar/is-tiff?)
           (lidar/rasters->index))))

(def residential-subtypes
  #{nil ;; no subtype = resi
    "" ;; blank string = resi
    "Residential"
    "House"
    "Apartments"
    "Detached"
    "Dormitory"
    "Terrace"
    "Houseboat"
    "Bungalow"
    "Static caravan"
    "Cabin"})

(def svm-range-limit 1.5)

(defn- load-svm [name]
  (-> name
      (io/resource)
      (slurp)
      (json/read-str :key-fn keyword-upcase-uscore)
      (svm/predictor)))

(defn- load-lm [name]
  (-> name
      (io/resource)
      (slurp)
      (json/read-str :key-fn keyword-upcase-uscore)
      (lm/predictor)))

(def svm-space-3d (load-svm "thermos_backend/importer/space-svm-3d.json"))
(def svm-space-2d (load-svm "thermos_backend/importer/space-svm-2d.json"))

(def lm-space-3d (load-lm "thermos_backend/importer/space-lm-3d.json"))
(def lm-space-2d (load-lm "thermos_backend/importer/space-lm-2d.json"))

(defn- run-svm-models [f sqrt-degree-days]
  (let [x (->> (for [[k v] f
                     :when (and (keyword? k)
                                (or (= :residential k)
                                    (and v (= "thermos-importer.lidar" (namespace k)))))]
                 [(keyword (name k))
                  (if (= :residential k) (boolean v) v)])
               (into {}))

        height-source (::lidar/height-source f)
        space-svm-3 (and (not= :default height-source)
                         (svm-space-3d x))
        svm-result  (or space-svm-3 (svm-space-2d x))
        sap-water   (sap/hot-water (:floor-area x))
        ]
    (or
     (when (and svm-result
                (> (aget svm-result 1) 1.5))
       (let [lm-value ((if space-svm-3 lm-space-3d lm-space-2d) x)
             lm-value (and lm-value (* lm-value sqrt-degree-days))]
        (when lm-value
          {:annual-demand (+ (max lm-value 3250.0) sap-water)
           :sap-water-demand sap-water
           :demand-source (if space-svm-3 "3d-lm" "2d-lm")})))

     (when svm-result
       {:annual-demand (+ (* sqrt-degree-days (aget svm-result 0)) sap-water)
        :sap-water-demand sap-water
        :demand-source (if space-svm-3 "3d-svm" "2d-svm")}))))

(def peak-constant 21.84)
(def peak-gradient 0.0004963)

(defn run-peak-model [annual-demand]
  (+ peak-constant (* annual-demand peak-gradient)))

(defn- inverse-peak-model [kwp]
  "In some data there is a known peak but no annual, and just point geometry.
  In this case, we can invert the peak regression to get an annual demand"
  [kwp]
  (when (number? kwp)
    (max 0.0 (/ (- kwp peak-constant) peak-gradient))))

(defn- blank-string? [x] (and (string? x) (string/blank? x)))

(defn- topo [{roads :roads buildings :buildings :as state} group-buildings]
  (let [crs (::geoio/crs roads)

        roads
        (topo/node-paths (::geoio/features roads)
                         :crs crs :snap-tolerance 5.0)

        roads ;; if group-buildings is geo-id, we need to give the
              ;; segments a unique ID within the map.
        (if (= :geo-id group-buildings)
          (map-indexed
           (fn [i road] (assoc road ::noded-id i)) roads)
          roads)

        [buildings roads]
        (topo/add-connections
         crs (::geoio/features buildings) roads
         :connect-to-connectors false
         :copy-field
         (when group-buildings
           (let [source-field
                 (if (= :geo-id group-buildings)
                   ::noded-id ;; see above.
                   group-buildings)
                 target-field :road-group]
             
             [source-field target-field])))

        ;; copy the subtype field into user-fields/category
        ;; this is there because it is how we tell connectors.
        ;; urgh.
        roads
        (for [road roads]
          (cond-> road
            (contains? road :subtype)
            (update-in [:user-fields "Category"] #(or % (:subtype road)))))
        
        ;; If there is a user-specified :group, it wins over what :road-group
        ;; got set to above. Otherwise we use :road-group as :group
        buildings
        (for [building buildings]
          (let [group (:group building)
                road-group (:road-group building)]
            (if (and road-group (not group))
              (assoc building :group road-group)
              building)))

        useful-groups
        (->> buildings
             (keep :group) ;; take groups
             (reduce ;; count how many there are in each case
              (fn [a g] (assoc a g (inc (get a g 0)))) {})
             (keep (fn [[g n]]
                     (when (and (> n 1)
                                (not (nil? g))
                                (not (blank-string? g)))
                       g)
                     )) ;; throw away singles, empty strings, etc
             (map-indexed (fn [i g] [g i])) ;; relabel from 0 to n-1
             (into {}))

        ;; relabel the groups in our buildings
        buildings (for [building buildings]
                    (update building :group useful-groups))
        ]
    (log/info (count (set (map :group buildings))) "building groups")
    (-> state
        (assoc-in [:roads     ::geoio/features] roads)
        (assoc-in [:buildings ::geoio/features] buildings))))

(defn dedup [state]
  (-> state
      (update-in [:buildings ::geoio/features] distinct-by ::geoio/id)
      (update-in [:roads ::geoio/features] distinct-by ::geoio/id)))

(defn- table->maps
  "Given a seq of seqs whose first element is a header,
  return a seq of maps from header values to row values"
  {:test #(test/is (= (table->maps [["this" "that"]
                                    [1 2]
                                    [3 4]])
                      (list {"this" 1 "that" 2}
                            {"this" 3 "that" 4})))}
  [[header & rows]]
  (map zipmap (repeat header) rows))

(defn- read-csv-file [file & {:keys [separator] :or {separator \,}}]
  (with-open [r (io/reader file)]
    (doall (table->maps (csv/read-csv r :separator separator)))))

(defn- compose-mapping
  "Composes a series of field mappings into a function which does them.
  A mapping is a literally a map which gives a renaming of a field.
  Non nil values of the key will be copied to the value"
  {:test
   #(let [c (compose-mapping {"widget" :sprocket
                              "blink" :blonk})]
      (test/is (= (get (c {"widget" 13 "blink" nil})
                       :sprocket) 13)))}
  [fields]
  (apply comp
         (for [[field-name field-purpose] fields]
           ;; special case for :user-fields - these are stored in a
           ;; blob together.
           (if (= :user-fields field-purpose)
             (fn [x]
               (assoc-in x
                         [:user-fields field-name]
                         (get x field-name)))
             
             (fn [x]
               (let [val (get x field-name)]
                 (if (and val
                          (or (not (string? val))
                              (not (string/blank? val))))
                   (assoc x field-purpose val)
                   x)))))))

(defn- compose-joins
  "Turns a list of joins and some tables into a function which runs the joins.

  Each join is given by a map keyed :table-file :table-column :gis-file :gis-column.
  The :table- parts are on the RHS and the :gis- parts are on the left.

  The tables should be a sequence of maps. Those which have table-column = gis-column will
  get merged onto the input.

  TODO FIXME we can end up replacing the keys in the LHS with keys from the RHS, which could break subsequent joins"
  {:test
   #(let [c (compose-joins
             [{:table-file "table"
               :table-column "a"
               :gis-column "b"}]
             {"table" [{"a" 1 :hello :world}
                       {"a" 2 :hello :i-love-you}]})
          ]

      (test/is (= :world (get (c {"b" 1}) :hello)))
      (test/is (= :i-love-you (get (c {"b" 2}) :hello))))}
  
  [joins tables]

  (apply comp
         (for [{table :table-file
                table-column :table-column
                gis-column :gis-column}
               joins
               :let [table (get tables table)]
               :when table
               :let [table (assoc-by table #(get % table-column))]]
           (fn [x]
             (merge x (get table (get x gis-column)))))))

(defn primary-file
  "Since some GIS formats have several parts, we have to put them in a directory.
  This function turns a file or directory into the important file therein"
  [file-or-directory]
  
  (let [file-or-directory (io/file file-or-directory)]
    (cond
      (.isDirectory file-or-directory)
      (let [contents (.listFiles file-or-directory)
            n (count contents)]
        (cond
          (zero? n) nil
          (= 1 n) (first contents)
          :else (first
                 (filter
                  #(has-extension % "shp")
                  contents))))
      (.isFile file-or-directory) file-or-directory)))

(defn- load-and-join
  "Load some geometry data - `files` names the input files, `joins`
  joins from tables to spatial data, and `mappings` gives the field mappings."
  [{files :files
    joins :joins
    mappings :mapping}
   legal-geometry-types]
  (let [files (for [[base {id :id}] files :when id]
                [base (primary-file (io/file (config :import-directory) id))])

        {gis-files true
         other-files false}
        (group-by (comp boolean
                        geoio/can-read?
                        second)
                  files)

        mappings (into {} (for [[file fields] mappings] [file (compose-mapping fields)]))
        
        other-files (for [[base file] other-files]
                      [base
                       (let [rows (case (file-extension file)
                                    "csv" (read-csv-file file)
                                    ("tab" "tsv") (read-csv-file file :separator \tab)
                                    [])
                             mapping (get mappings base identity)]
                         (map mapping rows))])
        
        other-files (into {} other-files)

        joins (group-by :gis-file joins)

        joins (for [[gis-file joins] joins]
                [gis-file (compose-joins joins other-files)])

        joins (into {} joins)
        
        all-features
        (mapcat
         (fn [[base file]]
           (let [features (::geoio/features
                           (geoio/read-from file
                                            :force-crs "EPSG:4326"
                                            :force-precision 100000000
                                            :key-transform identity))

                 features (filter
                           (comp legal-geometry-types ::geoio/type)
                           features)
                 
                 mapping (get mappings base identity)
                 join    (get joins base identity)]
             (map (comp join mapping) features)))
         gis-files)]
    {::geoio/crs "EPSG:4326" ::geoio/features all-features}))

(defn- produce-peak
  "Make sure the feature has a :peak-demand"
  [feature]
  (let [given-peak (as-double (:peak-demand feature))
        given-pbr  (as-double (:peak-base-ratio feature))]
    (cond
      given-peak
      (assoc feature
             :peak-demand given-peak
             :peak-source :given)
      
      given-pbr
      (assoc feature
             :peak-demand
             (let [demand-kw (annual-kwh->kw (:annual-demand feature))]
               (* (max given-pbr 1.0) demand-kw))
             :peak-source :ratio)
      
      :else
      (assoc feature
             :peak-demand (run-peak-model (:annual-demand feature))
             :peak-source :regression))))

(defn produce-cooling-demand
  "Make sure the feature has an :annual-cooling-demand"
  [feature cooling-benchmark]

  (let [given-demand (as-double (:annual-cooling-demand feature))
        given-peak   (as-double (:cooling-peak feature))
        
        given-benchmark-m (as-double (:cooling-benchmark-m feature))
        given-benchmark-c (as-double (:cooling-benchmark-c feature))
        floor-area (or (as-double (:floor-area feature)) 0)

        cooling-demand (cond
                         given-demand given-demand

                         (or given-benchmark-c given-benchmark-m)
                         (+ (or given-benchmark-c 0)
                            (* floor-area
                               (or given-benchmark-m 0)))
                         
                         :else
                         (* floor-area cooling-benchmark))
        
        cooling-peak (cond
                       given-peak given-peak
                       
                       :else
                       (cooling/cooling-peak cooling-demand (as-boolean (:residential feature true))))
        ]
    (assoc feature
           :annual-cooling-demand cooling-demand
           :cooling-peak          cooling-peak
           )))

(defn produce-heat-demand
  "Make sure the feature has an :annual-demand"
  [feature sqrt-degree-days]
  (let [given-demand (as-double (:annual-demand feature))
        
        minimum-demand (as-double (:minimum-annual-demand feature))
        maximum-demand (as-double (:maximum-annual-demand feature))
        
        given-floor-area (as-double (:floor-area feature))
        benchmark-m  (as-double (:benchmark-m feature))
        benchmark-c  (or (as-double (:benchmark-c feature))
                         (when benchmark-m 0))

        floor-area (or given-floor-area (::lidar/floor-area feature))

        residential (if (contains? feature :residential)
                      ;; the double boolean here means that if
                      ;; as-boolean says nil (which means 'doesn't look like a boolean')
                      ;; we just go false instead.
                      (boolean (as-boolean (:residential feature)))
                      (contains? residential-subtypes (:subtype feature)))

        feature    (assoc feature :residential residential)

        model-output (delay
                       (let [result (run-svm-models feature sqrt-degree-days)
                             annual-demand (:annual-demand result)]
                         (cond-> result
                           (and (number? minimum-demand)
                                (> minimum-demand annual-demand))
                           (assoc :annual-demand minimum-demand
                                  :demand-source :minimum)

                           (and (number? maximum-demand)
                                (> annual-demand maximum-demand))
                           (assoc :annual-demand maximum-demand
                                  :demand-source :maximum))))
        
        ;; produce demand
        feature (cond
                  given-demand
                  (assoc feature
                         :annual-demand given-demand
                         :demand-source :given)

                  benchmark-c
                  (assoc feature
                         :annual-demand (+ benchmark-c
                                           (* (or benchmark-m 0) floor-area))
                         :demand-source :benchmark)

                  (and (zero? (::lidar/footprint feature 0))
                       (number? (as-double (:peak-demand feature))))
                  (assoc feature
                         :annual-demand (inverse-peak-model
                                         (as-double (:peak-demand feature)))
                         :demand-source :inverse-peak)

                  :else
                  (merge feature @model-output))

        
        ]
    
    (assoc feature
           :sap-water-demand (sap/hot-water floor-area)
           )))

(defn- should-explode?
  "If a feature is going to end up with a summable prediction of demand,
  it should be exploded. Otherwise we can leave it alone."
  [feature]

  (and
   (#{:multi-polygon :multi-point} (::geoio/type feature))
   (not (or (:annual-demand feature)
            (and (:floor-area feature)
                 (or (:benchmark-m feature)
                     (:benchmark-c feature)))))))

(defn- explode-multi-polygons
  "Some of the features in the input may be multi-polygons.
  If they are, we want to break them into parts, so we can recombine
  them later."
  [features]
  (let [count-before (count features)
        features (->> features
                      (map-indexed (fn [i x] (assoc x ::id i)))
                      (mapcat (fn [feature]
                                (let [feature
                                      (-> feature
                                          (update :annual-demand as-double)
                                          (update :floor-area as-double)
                                          (update :benchmark-m as-double)
                                          (update :benchmark-c as-double))]
                                  (if (should-explode? feature)
                                    (geoio/explode-multi feature)
                                    [feature])))))
        count-after (count features)
        ]
    (log/info "Exploded multipolygons from" count-before "to" count-after)
    features))

(defn- explode-multi-lines [features]
  (reduce
   (fn [a f]
     (if (= :multi-line-string (::geoio/type f))
       (into a (geoio/explode-multi f))
       (conj a f)))
   []
   features))

(defn- merge-multi-polygon
  "Used by `merge-multi-polygons` to combine a bunch of features that
  have the same ::id, and so came from the same input feature."
  [polygons]
  (if (= 1 (count polygons))
    (first polygons)

    (let [basis (dissoc (first polygons) ::geoio/geometry)
          geoms (map ::geoio/geometry polygons)
          peaks (keep :peak-demand polygons)
          ]
      (cond-> (geoio/update-geometry
               basis
               (jts/create-multi-polygon geoms))

        (not= :given (:demand-source basis))
        (assoc :annual-demand (reduce + (map :annual-demand polygons)))

        ;; if the peak-source is given, we shouldn't sum them,
        ;; but also if the demand-source is given, since for a given
        ;; demand the first one has the right peak also.
        (not (or (= :given (:peak-source basis))
                 (= :given (:demand-source basis))
                 (empty? peaks)))
        (assoc :peak-demand (reduce + peaks))
        ))))

(defn merge-multi-polygons
  "The inverse of `explode-multi-polygons`, which see."
  [features]
  (->> features
       (group-by ::id)
       (map second)
       (map merge-multi-polygon)))

(defn add-areas [building]
  (let [height (double
                (or (:height building)
                    (::lidar/height building)
                    lidar/*storey-height*))]
    (-> building
        (assoc
         :wall-area   (::lidar/external-wall-area
                       building
                       (* (- (::lidar/perimeter building 0)
                             (::lidar/shared-perimeter building 0))
                          height))
         :floor-area  (::lidar/floor-area building 0)
         :ground-area (::lidar/footprint building 0)
         :roof-area   (::lidar/footprint building 0)
         :height      height)
        ;; (assoc-in [:user-fields "Height"] height)
        ;; (assoc-in [:user-fields "Height source"] (name (::lidar/height-source building)))

        )))

(defn- remove-zero-height [building]
  (let [storeys (:storeys building)
        height  (:height building)]
    (cond-> building
      (not (and (number? height) (pos? height) height))
      (dissoc :height)
      
      (not (and (number? storeys) (pos? storeys) storeys))
      (dissoc :storeys))))

(defn run-import
  "Run an import job enqueued by `queue-import`"
  [map-id project-id map-name parameters progress]
  (let [work-directory (util/create-temp-directory!
                        (config :import-directory)
                        (str
                         "map-" map-id "-"
                         (str/replace (or map-name "????") #"[^a-zA-Z0-9]+" "-") "-"))]
    (log/info "About to import" map-id map-name "in" (.getName work-directory))
    (with-open [log-writer (io/writer (io/file work-directory "log.txt"))]
      (binding [*out* log-writer]
        (pprint parameters)
        
        (try
          (let [geometry-source (-> parameters :geometry :source)
                
                osm-buildings   (= geometry-source :osm)
                osm-roads       (or osm-buildings (-> parameters :roads :include-osm))
                
                group-buildings (-> parameters :roads :group-buildings)
                progress* (fn [x p m] (progress :message m :percent p :can-cancel true) x)
                sqrt-degree-days (Math/sqrt (:degree-days parameters 2000.0))]
            (-> (cond-> {:work-directory work-directory}
                  ;; 1. Load any GIS files
                  (not osm-buildings)
                  (assoc :buildings
                         (load-and-join (merge (:geometry parameters)
                                               (:buildings parameters))
                                        #{:multi-polygon :polygon :point}))
                  

                  ;; (not osm-roads)
                  true ;; since osm-roads is now "load also"
                  (assoc :roads (load-and-join (merge (:geometry parameters)
                                                      (:roads parameters))
                                               #{:line-string :multi-line-string}))
                  
                  ;; 2. If we need some OSM stuff, load that
                  (or osm-buildings osm-roads)
                  (-> (progress* 10 "Querying OpenStreetMap")
                      (query-osm parameters)))

                ;; at this point, if we have multipolygons we should
                ;; explode them so that the LIDAR processing calculates
                ;; its stuff on a per-shape basis.

                ;; step 1: subdivide multi-features into bits
                (update-in [:buildings ::geoio/features] explode-multi-polygons)
                (update-in [:roads ::geoio/features] explode-multi-lines)
                ;; now we have several of everything, potentially
                (progress* 20 "Checking for LIDAR coverage")
                (update :buildings geoio/update-features :remove-zero-height remove-zero-height)
                (update :buildings lidar/add-lidar-to-shapes (load-lidar-index project-id))
                
                (progress* 30 "Computing annual demands")
                
                (as-> x
                    (let [;; choose a representative point
                          bounds (geoio/bounding-box (:buildings x) (geoio/bounding-box (:roads x)))

                          middle (.centre bounds)
                          
                          cooling-benchmark (try (cooling/cooling-benchmark (.getX middle) (.getY middle))
                                                 (catch Exception e
                                                   (log/error e "Error getting cooling benchmark, proceeding regardless with zero")
                                                   0))]
                      (log/info "Cooling bounds" bounds)
                      (log/info "Cooling benchmark" cooling-benchmark)
                      (update x :buildings geoio/update-features :produce-demands
                              #(-> %
                                   (produce-heat-demand sqrt-degree-days)
                                   (produce-cooling-demand cooling-benchmark)))))
                
                ;; at this point we need to recombine anything that has
                ;; been exploded.
                (update-in [:buildings ::geoio/features] merge-multi-polygons)

                ;; we want to do peak modelling afterwards
                (progress* 35 "Computing peak demands")
                (update :buildings geoio/update-features :produce-peaks produce-peak)
                
                (progress* 45 "De-duplicating geometry")
                (dedup)
                (progress* 50 "Noding paths and adding connectors")
                (topo group-buildings)
                (progress* 80 "Adding map to database")

                (update :buildings geoio/update-features :add-areas add-areas)))
          
          (catch Exception e
            (util/dump-error
             e "Error during import"
             :type "import" :data {:parameters parameters :map-id map-id})
            (throw e) ;; so the job gets marked failed
            ))))))

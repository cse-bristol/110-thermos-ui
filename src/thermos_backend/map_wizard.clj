;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.map-wizard
  "The state of the new-map wizard, and the rules for moving between its steps.

  The state is kept in the session while the user goes through the
  wizard. At the end it is stored as the map's :parameters, which is
  what thermos-backend.importer.process reads, so its shape matters:

  {:name, :description
   :geometry  {:source       :osm or :files
               :osm          {:osm-id \"relation:123\" :boundary <geojson>}
               :centroid     {:lat :lng}
               :map-position [lat0 lat1 lon0 lon1]
               :files        {base-name {:id :base-name :extensions :state
                                         :fields-by-type :count-by-type
                                         :centroid :extent}}}
   :buildings {:mapping {base-name {field-name field-keyword}}}
   :roads     {:mapping ... :include-osm :group-buildings}
   :degree-days, :hdd-from-server?, :project-id, :default-*-civil-cost}

  Geojson in here has string keys, as process.clj expects."
  (:require [clojure.string :as string]
            [clojure.set :refer [map-invert]]))

(def start-state
  {:name ""
   :description ""
   :geometry {:source :osm :files {}}
   :roads    {:include-osm false :group-buildings nil}
   :degree-days 2000
   :hdd-from-server? false
   :project-id nil
   :default-fixed-civil-cost 350.0
   :default-variable-civil-cost 700.0})

(def steps [:name :geometry :building-cols :road-cols :lidar :other-parameters])

(def building-geometry-type #{:polygon :point :multi-polygon})
(def road-geometry-type #{:line-string :multi-line-string})

(defn building-count [file]
  (reduce + 0 (vals (select-keys (:count-by-type file) building-geometry-type))))

(defn road-count [file]
  (reduce + 0 (vals (select-keys (:count-by-type file) road-geometry-type))))

(defn mappable-columns
  "A sorted list of [base-name field-name] for fields in the uploaded files
  on geometry of the given types."
  [{files :files} geometry-types]
  (->>
   (for [[_ meta] files
         g geometry-types
         f (get (:fields-by-type meta) g)]
     [(:base-name meta) f])
   (into #{})
   (sort)))

(defn next-page-map
  "A map from each step to the step after it, given the state."
  [state]
  (let [using-geometry-files (-> state :geometry :source (= :files))
        building-cols        (-> state :geometry (mappable-columns building-geometry-type))
        road-cols            (-> state :geometry (mappable-columns road-geometry-type))]
    (cond->
        {:name :geometry
         :geometry (if (and using-geometry-files (seq building-cols))
                     :building-cols
                     :lidar)
         :lidar :other-parameters}
      (and using-geometry-files (seq building-cols) (seq road-cols))        (assoc :building-cols :road-cols)
      (and using-geometry-files (seq building-cols) (not (seq road-cols)))  (assoc :building-cols :lidar)
      (and using-geometry-files (seq road-cols))                            (assoc :road-cols :lidar))))

(defn next-step [state step]
  (get (next-page-map state) step))

(defn previous-step [state step]
  (get (map-invert (next-page-map state)) step))

(defn- too-large [boundary]
  (try
    (let [geo (or (get-in boundary ["geometry" "coordinates"])
                  (get-in boundary ["coordinates"]))
          ring (first geo)
          xs (map first ring)
          ys (map second ring)
          dx (- (reduce max xs) (reduce min xs))
          dy (- (reduce max ys) (reduce min ys))]
      (or (> dx 0.15)
          (> dy 0.15)))
    (catch Exception e)))

(defn- validate-osm-area [geo]
  (cond
    (and (= :osm (:source geo))
         (not (or (:osm-id (:osm geo))
                  (:boundary (:osm geo)))))
    ["Select an area on the map to continue"]

    (and (= :osm (:source geo))
         (:boundary (:osm geo))
         (too-large (:boundary (:osm geo))))
    ["Select a smaller area on the map to continue"]))

(defn- validate-files [geo]
  (when (= :files (:source geo))
    (let [files (vals (:files geo))]
      (cond
        (zero? (reduce + 0 (map building-count files)))
        ["Upload some GIS files with polygons or multipolygons to continue"]

        (some (comp #{:invalid :error} :state) files)
        ["Remove any invalid files or failed uploads to continue"]))))

(defn requirements
  "Things that need doing before you can go on from `step`"
  [state step]
  (case step
    :name (when (string/blank? (:name state))
            ["Enter a name for your map"])
    :geometry (concat (validate-files    (:geometry state))
                      (validate-osm-area (:geometry state)))
    nil))

(defn centroid
  "The centroid of the map area, either from the OSM area or as the average
  of the uploaded files' centroids."
  [state]
  (case (get-in state [:geometry :source])
    :osm (get-in state [:geometry :centroid])
    :files (let [centroids (keep :centroid (vals (get-in state [:geometry :files])))]
             (when (seq centroids)
               (merge-with /
                           (apply merge-with + centroids)
                           {:lat (count centroids) :lng (count centroids)})))
    nil))

(defn extent-geojson
  "The extent of the map area, either from the OSM area or as the extent of
  all the uploaded files."
  [state]
  (case (get-in state [:geometry :source])
    :osm (get-in state [:geometry :osm :boundary])
    :files (let [extents (keep :extent (vals (get-in state [:geometry :files])))]
             (when (seq extents)
               (let [x1 (apply min (map :x1 extents))
                     y1 (apply min (map :y1 extents))
                     x2 (apply max (map :x2 extents))
                     y2 (apply max (map :y2 extents))]
                 {"type" "Feature"
                  "properties" {"bounds" {"x1" x1 "y1" y1 "x2" x2 "y2" y2}}
                  "geometry" {"type" "Polygon"
                              "coordinates" [[[x1 y1]
                                              [x2 y1]
                                              [x2 y2]
                                              [x1 y2]
                                              [x1 y1]]]}})))
    nil))

(defn- file-extension [filename]
  (let [i (.lastIndexOf filename ".")]
    (if (neg? i) "" (string/lower-case (subs filename i)))))

(defn base-name [filename]
  (let [i (.lastIndexOf filename ".")]
    (if (neg? i) filename (subs filename 0 i))))

(defn group-uploads
  "Group uploaded files (ring multipart maps) by their name without the
  extension, and check each group is something we can read. Returns
  a map from base-name to {:base-name :extensions :uploads} and maybe
  :state :invalid and a :message."
  [uploads]
  (into {}
        (for [[base group] (group-by (comp base-name :filename) uploads)]
          (let [extensions (mapv (comp file-extension :filename) group)
                ext (set extensions)]
            [base
             (cond->
                 {:base-name base
                  :extensions extensions
                  :uploads group}

               (not (some ext [".shp" ".json" ".geojson" ".gpkg" ".geopackage"]))
               (assoc :state :invalid :message "Unsupported file type")

               (and (ext ".shp")
                    (not (and (ext ".shx") (ext ".dbf") (ext ".prj"))))
               (assoc :state :invalid :message "Missing component of shapefile"))]))))

(def road-fields
  [{:value :identity :label "Identity (text)"
    :doc
    [:span "An identifier - these are stored on the roads in the database and visible in downloaded GIS files."]}

   {:value :user-fields :label "User-defined field (any)"
    :doc
    [:span "Any other field you want to keep, like a classification, address, etc."]}])

(def building-fields
  [{:value :annual-demand
    :label "Annual heat demand (kWh/yr)"
    :doc
    [:span
     "A value for annual demand will be used in preference to any other estimate. "
     "Otherwise, a benchmark estimate will be used if available, or the built-in regression model otherwise."]}

   {:value :maximum-annual-demand
    :label "Max. heat demand (kWh/yr)"
    :doc
    [:span "An upper bound to apply to the modelled annual heat demand. If the demand estimation model is used, and it produces an annual demand above this value, this value will be used instead."]}

   {:value :minimum-annual-demand
    :label "Min. heat demand (kWh/yr)"
    :doc
    [:span "A lower bound to apply to the modelled annual heat demand. If the demand estimation model is used, and it produces an annual demand below this value, this value will be used instead."]}

   {:value :annual-cooling-demand
    :label "Annual cooling demand (kWh/yr)"
    :doc
    [:span
     "A value for annual cooling demand will be used in preference to a cooling estimate."]}

   {:value :cooling-peak
    :label "Peak cooling demand (kW)"
    :doc
    [:span
     "A value for peak cooling demand will be used in preference to an estimate."]}

   {:value :peak-demand :label "Peak heat demand (kW)"
    :doc
    [:span
     "A value for peak demand will be used in preference to any other estimate. "
     "Otherwise, the peak/base ratio will be applied to the demand if available, or the build in regression model otherwise."]}

   {:value :height :label "Building height (m)"
    :doc
    [:span
     "A value for building height will be used in preference to any LIDAR data on the server."
     [:br]
     "Building height will improve the quality of any demand estimates produced from the built-in regression model."]}

   {:value :fallback-height :label "Fallback building height (m)"
    :doc
    [:span
     "A value for building height to be used if LIDAR data on the server and the building height field are missing."]}

   {:value :floor-area :label "Floor area (m2)"
    :doc
    [:span "A value for floor area will be used in benchmark-based estimates. "
     "If no value is provided, a value will be estimated from the building geometry and height (if known)."]}

   {:value :benchmark-c :label "Heat benchmark (kWh/yr)"
    :doc
    [:span
     "A constant benchmark - this is used in combination with the variable benchmark term. "
     "If a building has associated benchmarks and no specified demand, demand will be estimated as this constant plus floor area times the variable benchmark."]}

   {:value :benchmark-m :label "Heat benchmark (kWh/m2/yr)"
    :doc [:span "A variable benchmark per floor area."]}

   {:value :cooling-benchmark-c :label "Cooling benchmark (kWh/yr)"
    :doc
    [:span
     "A constant benchmark - this is used in combination with the variable benchmark term. "
     "If a building has associated benchmarks and no specified demand, demand will be estimated as this constant plus floor area times the variable benchmark."]}

   {:value :cooling-benchmark-m :label "Cooling benchmark (kWh/m2/yr)"
    :doc [:span "A variable benchmark per floor area."]}

   {:value :peak-base-ratio :label "Peak/base ratio"
    :doc
    [:span "If present, and no peak demand value is known, the peak demand will be estimated as the annual demand (converted into kW) multiplied with this factor."
     [:br]
     "Otherwise, a built in regression will be used."]}

   {:value :connection-count :label "Connection count"
    :doc
    [:span "The number of end-user connections the building contains. This affects only the application of diversity curves within the model."]}

   {:value :identity :label "Identity (text)"
    :doc
    [:span "An identifier - these are stored on the buildings in the database and visible in downloaded GIS files."]}

   {:value :user-fields :label "User-defined field (any)"
    :doc
    [:span "Any other field you want to keep, like a classification, address, etc."]}

   {:value :residential :label "Residential (logical)"
    :doc
    [:span "A logical value, represented either as a boolean column, or the text values yes, no, true, false, 1 or 0. "
     "If available, this will improve the quality of built-in regression model results. Otherwise, this is assumed to be true."]}

   {:value :group :label "Building group (any value)"
    :doc
    [:span "Any buildings which have the same value in this field (except null or an empty string) will be grouped together. "
     "Buildings in a group must all be connected to a network at once, or not at all."]}])

(def field-steps
  "The steps where fields get given meanings, with what they need to know."
  {:building-cols {:label "buildings"
                   :key :buildings
                   :geometry-types building-geometry-type
                   :options building-fields}
   :road-cols     {:label "roads"
                   :key :roads
                   :geometry-types road-geometry-type
                   :options road-fields}})

(defn columns-for-step [state step]
  (mappable-columns (:geometry state) (:geometry-types (field-steps step))))

(defn set-mapping
  "Update the field mapping for `step` from `selections`, which is a list of
  option names (or \"none\") in the same order as `columns-for-step`."
  [state step selections]
  (let [{:keys [key options]} (field-steps step)
        valid (into {} (for [o options] [(name (:value o)) (:value o)]))]
    (reduce
     (fn [state [[file field] selection]]
       (if-let [value (valid selection)]
         (assoc-in state [key :mapping file field] value)
         (update-in state [key :mapping file] dissoc field)))
     state
     (map vector (columns-for-step state step) selections))))

(defn guess-mapping
  "Guess what the fields for `step` mean by their names. If `harder`,
  fields we can't guess are kept as user fields. Fields which have
  already been given a meaning are left alone."
  [state step harder]
  (let [{:keys [key options]} (field-steps step)
        option-names (into {} (for [o options] [(name (:value o)) (:value o)]))
        guess (fn [field-name]
                (or (-> field-name
                        (string/trim)
                        (string/lower-case)
                        (string/replace #"[^a-z0-9]+" "-")
                        (->> (get option-names)))
                    (and harder :user-fields)))]
    (reduce
     (fn [state [file field]]
       (let [guessed (guess field)]
         (if (and guessed (nil? (get-in state [key :mapping file field])))
           (assoc-in state [key :mapping file field] guessed)
           state)))
     state
     (columns-for-step state step))))

(defn parameters
  "The map parameters to store and give to the importer."
  [state]
  (update-in state [:geometry :files]
             (fn [files]
               (into {} (for [[k f] files] [k (dissoc f :uploads)])))))

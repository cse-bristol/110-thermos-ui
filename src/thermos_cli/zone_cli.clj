(ns thermos-cli.zone-cli
  "Restricted CLI tool for use in zoning work only

  INPUT FILE FORMAT
  A geopackage having tables

  buildings:
    building_id
    heat_demand_kwh
    peak_demand_kw
    num_customers
    meta (json string)
    classification
    abp_class
    height

  paths:
    path_id
    cost_category
    meta etc.

  PARAMETERS FORMAT
  An edn file containing a map having keys
    
  
  OUTPUT FILE FORMAT
  A geopackage, having tables:

  buildings:
    building_id
    etc

  paths:
    path_id
    etc
  
  "
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [clojure.string :as string]
            [thermos-importer.geoio :as geoio]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.path :as path]
            [thermos-specs.demand :as demand]
            [thermos-importer.spatial :as spatial]
            [thermos-importer.lidar :as lidar]
            [thermos-cli.noder :as noder]
            [thermos-cli.core :refer [select-top-n-supplies]]
            [thermos-backend.importer.process :as importer]
            [thermos-specs.defaults :as defaults]
            [thermos-specs.document :as document]
            [thermos-specs.tariff :as tariff]
            [thermos-backend.solver.interop :as interop]
            [thermos-specs.measure :as measure]
            [thermos-specs.supply :as supply]
            [clojure.data.json :as json]
            [clojure.set :as set]
            [thermos-specs.solution :as solution]
            [cljts.core :as jts]
            [mount.core :as mount])
  (:import [java.sql Connection DriverManager SQLException
            PreparedStatement Types])
  (:gen-class))

(def options
  [["-i" "--input-file FILE"
    "The input file; this should be a geopackage having the expected form"
    :parse-fn io/file
    :validate [#(let [f (io/file %)] (and (.exists f) (.isFile f))) "File not found"]]
   ["-o" "--output-file FILE"
    "The output file; this will be a geopackage with a defined set of tables"
    :parse-fn io/file
    :validate [#(let [f (io/file %)] (or (not (.exists f)) (.isFile f))) "Cannot overwrite directory"]]
   ["-p" "--parameters FILE"
    "An EDN file full of parameters"
    :validate [#(let [f (io/file %)] (and (.exists f) (.isFile f))) "File not found"]]
   [nil "--heat-price X"
    "The heat price to use when doing network model.
If not given, does the base-case instead (no network)."
    :parse-fn #(Double/parseDouble %)]
   [nil "--output-geometry"
    "Output geometry into all tables, for debug"]
   [nil "--round-and-evaluate"
    "Indicates the input file is the EDN for a previously solved problem, which should just be loaded, rounded, and reevaluated"
    ]
   ["-h" "--help" "This"]])

(defn- exit-with [message code]
  (binding [*out* *err*] (println message))
  ;; (System/exit code)
  )

(defn- usage-message [summary] (str "Usage:\n" summary))
(defn- error-message [errors]  (str "Invalid arguments:\n" (string/join \newline errors)))

(defmethod print-method org.locationtech.jts.geom.Geometry
  [this ^java.io.Writer w]
  (.write w "#geojson ")
  (print-method (jts/geom->json this) w))

(defn- read-edn [file]
  (with-open [r (java.io.PushbackReader. (io/reader (io/as-file file)))]
    (edn/read {:readers {'geojson jts/json->geom}} r)))

(declare run-with run-optimiser round-solution)

(defn -main [& arguments]
  (mount/start-with {#'thermos-backend.config/config {}})
  (let [{:keys [options arguments summary errors]}
        (parse-opts arguments options)]
    (cond
      (:help options)
      (exit-with (usage-message summary) 0)

      (seq errors)
      (exit-with (error-message errors) 1)

      (not (empty? arguments))
      (exit-with (format "Unexpected arguments: %s" arguments) 2)
      
      :else
      (run-with options))))

(defn- run-with [{:keys [input-file output-file parameters heat-price
                         round-and-evaluate
                         output-geometry]
                  :as options}]
  {:pre [(and input-file parameters output-file)]}
  (when (.exists output-file)
    (io/delete-file output-file))

  (if round-and-evaluate
    (round-solution options)
    (run-optimiser options)))

;; The real stuff follows

(defn- line? [feature]
  "is the feature a line feature"
  (-> feature ::geoio/type #{:line-string :multi-line-string} boolean))

(defn- noder-options [parameters]
  {:shortest-face  3.0
   :snap-tolerance 0.1
   :trim-paths     true
   :transfer-field :path-segment})

(defn- make-candidate-path [parameters p]
  (merge
   (json/read-str (get p "meta" "{}"))
   (dissoc p "meta")
   {::candidate/type      :path
    ::candidate/inclusion :optional
    ::path/length         (::spatial/length p)
    ::path/start          (::geoio/id (::spatial/start-node p))
    ::path/end            (::geoio/id (::spatial/end-node p))
    :connector            (:connector p)
    ::candidate/geometry  (::geoio/geometry p)
    ::candidate/id        (::geoio/id p)

    "id"                  (get p "id")
    }))

(defn- make-candidate-building [parameters b]
  (merge
   (json/read-str (get b "meta" "{}"))
   (dissoc b "meta")
   {::candidate/id          (::geoio/id b)
    ::candidate/type        :building
    ::candidate/inclusion   :optional
    ::candidate/wall-area   (:wall-area b)
    ::candidate/roof-area   (:roof-area b)
    ::candidate/ground-area (:ground-area b)
    ::candidate/connections (::spatial/connects-to-node b)
    ::candidate/geometry    (::geoio/geometry b)
    
    ::demand/kwh              (get b "heat_demand_kwh")
    ::demand/kwp              (get b "peak_demand_kw")
    ::demand/connection-count (or (get b "num_customers") 1)
    
    :path-segment             (:path-segment b)
    "id"                      (get b "id")
    }))

(defn- make-candidates [parameters paths buildings]
  ;; In node-connect in noder.clj, we have associated :path-segment to
  ;; each path, which is a unique int for each noded segment of input
  ;; road. This will be propagated onto connectors as well according
  ;; to the segment they connect to.

  ;; We use this to put "connects_to" onto paths and buildings as well
  ;; which is the toid of the original road they connect to (a bit
  ;; less specific than the :path-segment value)
  (let [road-segment-to-road-toid
        (reduce
         (fn [out path]
           (let [segment (:path-segment path)
                 toid    (get path "id")]
             (cond-> out
               (and segment toid)
               (assoc segment toid))))
         {}
         paths)]
    (as-> {} cs
      (reduce
       (fn [cs p]
         (let [p (-> (make-candidate-path parameters p)
                     (assoc "connects_to"
                            (get road-segment-to-road-toid (:path-segment p))))]
           (cond-> cs p (assoc (::candidate/id p) p))))
       cs paths)
      (reduce
       (fn [cs b]
         (let [b (-> (make-candidate-building parameters b)
                     (assoc "connects_to"
                            (get road-segment-to-road-toid (:path-segment b))
                            :path-id
                            (get road-segment-to-road-toid (:path-segment b))))]
           (cond-> cs b (assoc (::candidate/id b) b))))
       cs buildings))))

(defn- sconj [s x] (set (conj s x)))

(defn- matches-rule
  "Horrible rule engine
   options for rules are

  - :default => always matches
  - [:and | :or | :not & rules] => obvious
  - [:is FIELD X1 X2 X3]
  - [:demand< | :peak< X]
  "
  [candidate rule]
  (if (= :default rule) true
      (let [[op & args] rule]
        (case op
          :and     (every? #(matches-rule candidate %) args)
          :or      (some #(matches-rule candidate %) args)
          :not     (not (matches-rule candidate (first args)))
          :in      (let [[field & values] args]
                     (contains? (set values) (get candidate field)))
          (:demand< :peak<)
          (let [threshold (first args)
                x         (get candidate
                               (if (= op :demand<) ::demand/kwh ::demand/kwp))]
            (< x threshold))
          
          false))))

(defn- round-groups
  "Apply rounding to the groups of buildings in `problem`

  Return a tuple [rounding decisions, fixed problem]
  "
  [problem parameters]
  (let [buildings (filter (comp #{:building} ::candidate/type)
                          (vals (::document/candidates problem)))

        group-field                (:rounding/group-buildings-by parameters)
        [group-method lower upper] (:rounding/round-groups-by parameters)

        [lower upper]
        (cond
          (not (or lower upper)) [0.5 0.5]
          (not lower)            [upper upper]
          (not upper)            [lower lower]
          :else                  [lower upper])

        get-group-value (case group-method
                          :kwh           ::demand/kwh
                          :kwp           ::demand/kwp
                          :address-count ::demand/connection-count
                          :building-count (constantly 1))
        
        groups (reduce
                (fn [acc building]
                  (let [group     (get building group-field)
                        connected (boolean (::solution/connected building))
                        value     (get-group-value building)
                        ]
                    (-> acc
                        (update-in [group connected]
                                   (fn [x y] (+ (or x 0) (or y 0))) value)
                        (update-in [group :n] #(inc (or % 0))))))
                {}
                buildings)

        decisions (->>
                   (for [[group {v-on true v-off false}] groups]
                     (let [num (or v-on 0)
                           den (+ (or v-on 0) (or v-off 0))
                           p (/ num den)]
                       [group (cond
                                (< p lower)  :down
                                (>= p upper) :up
                                :else        :skip)]))
                   (into {}))


        problem
        (document/map-buildings
         problem
         (fn [building]
           (if (candidate/has-supply? building)
             (assoc building
                    ::candidate/inclusion
                    ::rounded-building "skip"
                    (if (candidate/is-connected? building) :required :optional))

             (let [group (-> (get building group-field)
                             (str)
                             (string/trim))
                   group (if (string/blank? group) nil group)
                   decision (if (nil? group) :skip (get decisions group :skip))
                   inclusion (case decision
                               :up :required
                               :down :optional
                               :skip (if (candidate/is-connected? building)
                                       :required :optional))

                   building (assoc building
                                   ::candidate/inclusion inclusion
                                   ::rounding-group   (and group (str group))
                                   ::rounded-building (if (or (and (= inclusion :required)
                                                                   (candidate/is-connected? building))
                                                              (and (= inclusion :optional)
                                                                   (not (candidate/is-connected? building))))
                                                        "skip"
                                                        (name decision)))

                   building (if (= :optional inclusion)
                              (let [;; unplug from network
                                    building    (assoc building ::candidate/connections nil)
                                    ;; see if we can pin the alternative
                                    alternative (::solution/alternative building)]
                                (cond
                                  ;; force the counterfactual decision
                                  (:counterfactual alternative)
                                  (assoc building ::demand/alternatives nil)

                                  ;; force this alternative
                                  alternative
                                  (assoc building
                                         ::demand/alternatives #{(::supply/id alternative)}
                                         ::demand/counterfactual nil)

                                  :else building))
                              
                              ;;leave it alone
                              building)
                   ]
               building ))))]
    [(for [[group stats] groups]
       (assoc stats
              :decision (get decisions group)
              :group group))
     problem]))

(defn- assign-by-rules
  "Given a single `rule` that maps to `ids`, test if `candidate`
  matches the rule. If it does, return `candidate` with `ids` assoced under `key`

  if `multi` is true then a non-coll `ids` is lifted to the set #{ids}"
  [multi key candidate [rule ids]]
  (cond-> candidate
    (matches-rule candidate rule)
    (-> (assoc key (if multi (if (coll? ids) (set ids)
                                 #{ids})
                       (if (coll? ids)
                         (throw (ex-info "Invalid rule (has multiple outputs)"
                                         {:rule rule
                                          :ids ids
                                          :key key}))
                         ids)))
        (reduced))))

(defn- assign-building-options [candidate insulation-rules alternative-rules connection-cost-rules]
  (as-> candidate c
    (reduce (partial assign-by-rules true ::demand/insulation)   c insulation-rules)
    (reduce (partial assign-by-rules true ::demand/alternatives) c alternative-rules)
    (reduce (partial assign-by-rules false ::tariff/cc-id)       c connection-cost-rules)))

(defn- assign-civil-cost [candidate civils-rules]
  (reduce (partial assign-by-rules false ::path/civil-cost-id) candidate civils-rules))

(defn- set-requirement [candidate requirement-rules]
  (reduce (partial assign-by-rules false ::candidate/inclusion)
          (assoc candidate ::candidate/inclusion :optional)
          requirement-rules))

(defn add-supply-points
  "Modify `instance` to have `n` supply points with the given
  `supply-parameters` in each of its connected components" 
  [instance n supply-parameters]
  (select-top-n-supplies
   instance
   supply-parameters
   n
   (not (contains? supply-parameters ::supply/capacity-kwp))))

(let [geometry-types #{:geometry :geometry-collection
                       :line-string :polygon :point
                       :multi-polygon :multi-line-string :multi-point}
      
      type-name (reduce
                 (fn [a gt]
                   (assoc a gt
                          (string/join
                           (map string/capitalize (string/split (name gt) #"-")))))

                 {:string "String"
                  :int "Integer"
                  :double "Double"
                  :boolean "Boolean"}
                 
                 geometry-types
                 )]
  (defn write-sqlite
    "Write data into an sqlite database
  `file` will be coerced to a file.
  `table` is the name of a table
  `columns` is triples [name type accessor]
  "
    [file table columns rows & {:keys [block-size crs] :or {block-size 5000}}]

    (if-let [geometry-column (first (filter (comp geometry-types second) columns))]
      ;; delegate to geoio which can write geometry but has different interface.
      
      (geoio/write-to
       {::geoio/crs (or crs "EPSG:4326") ::geoio/features rows}
       file
       
       :fields          (->>
                         (for [[col type get-val] columns]
                           [col {:type (type-name type) :value get-val}])
                         (into {}))

       :table-name      table
       :geometry-column (first geometry-column))
      
      (let [quote-name (fn [s] (str "\"" s "\""))

            column-setters
            (for [[ix [_ type get-val]] (map-indexed vector columns)]
              (let [ix (inc ix)]
                (case type
                  :int
                  (fn set-int [^PreparedStatement ps row]
                    (if-let [val (get-val row)]
                      (.setInt ps ix (int val))
                      (.setNull ps ix Types/INTEGER)))
                  
                  :double
                  (fn set-double [^PreparedStatement ps row]
                    (if-let [val ^double (get-val row)]
                      (.setDouble ps ix (int val))
                      (.setNull ps ix Types/REAL)))
                  
                  :boolean
                  (fn set-bool [^PreparedStatement ps row]
                    (.setBoolean ps ix (boolean (get-val row))))

                  :blob
                  (fn set-blob [^PreparedStatement ps row]
                    (if-let [v ^bytes (get-val row)]
                      (.setBytes ps ix v)
                      (.setNull ps ix Types/BLOB)))
                  
                  (fn set-str [^PreparedStatement ps row]
                    (if-let [val (get-val row)]
                      (.setString ps ix (if (or (keyword? val) (symbol? val))
                                          (name val) (str val)))
                      (.setNull ps ix Types/CHAR))))))
            
            column-ddl (string/join
                        ", "
                        (for [[col ctype _] columns]
                          (str (quote-name col) " "
                               (case ctype
                                 :int     "INTEGER"
                                 :double  "REAL"
                                 :boolean "BOOLEAN"
                                 :blob    "BLOB"

                                 "TEXT"))))
            insert
            (format "INSERT INTO \"%s\" (%s) values (%s)"
                    table
                    (string/join ", " (for [[col _ _] columns] (quote-name col)))
                    (string/join ", " (repeat (count columns) "?")))
            ]
        (with-open [conn (DriverManager/getConnection
                          (format "jdbc:sqlite:%s" (.getCanonicalPath (io/as-file file))))
                    cts  (.createStatement conn)]
          (.execute
           cts
           (format "CREATE TABLE IF NOT EXISTS \"%s\" (%s);"
                   table column-ddl))

          (with-open [ps ^PreparedStatement (.prepareStatement conn insert)]
            (doseq [row rows]
              (doseq [f column-setters] (f ps row))
              (.addBatch ps))
            (.executeBatch ps)))))))

(defn gzip-string
  "Gzip a string into a byte array"
  ^bytes [^String s]
  (let [baos (java.io.ByteArrayOutputStream.)]
    (with-open [gzos (java.util.zip.GZIPOutputStream. baos)]
      (.write gzos (.getBytes s java.nio.charset.StandardCharsets/UTF_8)))
    (.toByteArray baos)))

(defn gzip-edn [thing]
  (gzip-string (with-out-str (prn thing))))

(defn- print-summary [problem]
  (def -last-thing problem)
  (let [alternatives (::document/alternatives problem)
        insulation   (::document/measures problem)
        civils       (:civils (::document/pipe-costs problem))
        con-cost     (::document/connection-costs problem)
        candidates   (vals (::document/candidates problem))]

    (println "Alternatives:"
             (frequencies
              (map (comp ::supply/name alternatives)
                   (mapcat ::demand/alternatives candidates))))

    (println "Insulations:"
             (frequencies
              (map (comp ::measure/name insulation)
                   (mapcat  ::demand/insulation candidates))))

    (println "Civils:"
             (frequencies
              (keep (comp civils ::path/civil-cost-id) candidates)))

    (println "Con. cost:"
             (frequencies
              (keep (comp ::tariff/name con-cost ::tariff/cc-id) candidates)))
    ))


(let [building-id #(get % "id")
      path-id     #(get % "id")

      heating-system-capex
      #(+ (-> % ::solution/alternative :capex (:principal 0))
          (-> % ::solution/connection-capex   (:principal 0)))

      heating-system-opex
      #(-> % ::solution/alternative :opex (:annual 0))
      
      heating-system-fuel
      #(-> % ::solution/alternative :heat-cost (:annual 0))

      insulation-m2   #(reduce + 0 (keep :area (::solution/insulation %)))
      insulation-capex #(reduce + 0 (keep :principal (::solution/insulation %)))

      supply-capex     #(-> % ::solution/supply-capex (:principal 0))
      supply-opex      #(-> % ::solution/supply-opex (:annual 0))
      supply-heat-cost #(-> % ::solution/heat-cost (:annual 0))

      path-capex       #(:principal (::solution/pipe-capex %))]
  (defn- output [problem buildings paths supplies
                 output-file crs output-geometry]
    (let [mode (document/mode problem)
          heat-demand-kwh #(candidate/solved-annual-demand % mode)
          peak-demand-kw  #(candidate/solved-peak-demand % mode)

          insulation-kwh  #(- (candidate/annual-demand % mode)
                              (candidate/solved-annual-demand % mode))
          
          civil-cost-name  #(document/civil-cost-name problem (::path/civil-cost-id %))


          ]
      
      (write-sqlite
       output-file
       "buildings"
       (into [["building_id"          :string  building-id]
              ["rounded_building"     :string  ::rounded-building]
              ["rounding_group"       :string  ::rounding-group]
              ["connects_to"          :string  #(get % "connects_to")]
              ["on_network"           :boolean ::solution/connected]
              ["heating_system"       :string  candidate/solution-description]
              ["heating_system_capex" :double  heating-system-capex]
              ["heating_system_opex"  :double  heating-system-opex]
              ["heating_system_fuel"  :double  heating-system-fuel]
              ["heat_demand_kwh"      :double  heat-demand-kwh]
              ["peak_demand_kw"       :double  peak-demand-kw]
              ["insulation_kwh"       :double  insulation-kwh]
              ["insulation_m2"        :double  insulation-m2]
              ["insulation_capex"     :double  insulation-capex]
              ]
             (when output-geometry
               [["geometry"     :polygon ::candidate/geometry]]))
       
       buildings
       :crs crs
       )

      (write-sqlite
       output-file
       "cluster_supplies"
       (into [["building_id"       :string building-id]
              ["peak_output_kw"    :double ::solution/capacity-kw]
              ["heat_output_kwh"   :double ::solution/output-kwh]
              ["supply_capex"      :double supply-capex]
              ["supply_opex"       :double supply-opex]
              ["supply_heat_cost"  :double supply-heat-cost]]
             (when output-geometry
               [["geometry"     :polygon ::candidate/geometry]]))
       supplies
       :crs crs)
      
      (write-sqlite
       output-file
       "paths"
       [["geometry"     :line-string ::candidate/geometry]
        ["path_id"      :string      path-id]
        ["connects_to"  :string      #(get % "connects_to")]
        ["is_connector" :boolean     :connector]
        ["length"       :double      ::path/length]
        ["on_network"   :boolean     candidate/in-solution?]
        ["civil_cost"   :string      civil-cost-name]
        ["path_capex"   :double      path-capex]
        ["diameter"     :double      ::solution/diameter-mm]
        ["capacity_kw"  :double      ::solution/capacity-kw]
        ["losses_kwh"   :double      ::solution/losses-kwh]
        ["diversity"    :double      ::solution/diversity]
        ["unreachable"  :string      ::solution/unreachable]]
       paths
       :crs crs))))

(defn- output-metadata [solution rounded output-file]
  (write-sqlite
     output-file
     "meta"
     [["rounded" :boolean  (constantly rounded)]
      ["runtime" :double   ::solution/runtime]
      ["objective" :double ::solution/objective]
      ["gap" :double       (fn [x] (when-let [g (::solution/gap x)]
                                     (* g 100.0)))]
      ["state" :string     (comp name ::solution/state)]
      ["message" :string   ::solution/message]
      ["iterations" :int   ::solution/iterations]
      ["lower_bound" :double (comp first ::solution/bounds)]
      ["upper_bound" :double (comp second ::solution/bounds)]
      ["edn" :blob (fn [x] (gzip-edn (dissoc x ::solution/log)))]
      ["log" :blob (fn [x] (gzip-string (::solution/log x)))]]

     [solution]))

(defn- run-optimiser [{:keys [input-file output-file parameters heat-price
                              output-geometry]
                       :as options}]
  (let [parameters (read-edn parameters)

        input-features
        (cond-> (geoio/read-from input-file :key-transform identity)
          ;; remove all paths, if no heat price - that is counterfactual
          (not heat-price)
          (update ::geoio/features #(remove line? %)))

        input-features (update input-features
                               ::geoio/features
                               #(filter
                                 (fn [x]
                                   (or (line? x)
                                       (and (number? (get x "heat_demand_kwh"))
                                            (number? (get x "peak_demand_kw")))))
                                 %))
        
        _ (println (count (::geoio/features input-features)) "features in input")
        
        ;; if we want to filter out some paths we need to do it here
        ;; or inside noder. If we do it here, they will be missing from
        ;; the output dataset, which is ~ok~?

        requirement-rules (:thermos/requirement-rules parameters)
        
        input-features
        (let [forbidding-rules
              (keep
               (fn [[rule k]] (when (= k :forbidden) rule))
               requirement-rules)]
          (update
           input-features
           ::geoio/features
           (fn [features]
             (remove
              (fn [feature]
                (some
                 (fn [rule] (matches-rule feature rule))
                 forbidding-rules))
              features))))

        [paths buildings] (noder/node-connect input-features (noder-options parameters))

        _ (println (count buildings) "/" (count paths)
                   "buildings / paths")

        ;; do some area calculations for measures to work right
        buildings
        (->> #:thermos-importer.geoio
             {:features
              (for [b buildings] (assoc b :height (get b "height")))
              :crs (::geoio/crs input-features)}
             (lidar/add-other-attributes)
             (::geoio/features) ;; and get it out again
             (map importer/add-areas))
        
        candidates (make-candidates parameters paths buildings)

        pipe-costs                     (:thermos/pipe-costs parameters)

        insulation                     (->> (for [[id x] (:thermos/insulation parameters)]
                                              [id (assoc x ::measure/id id)])
                                            (into {}))
        
        alternatives                   (->> (for [[id x] (:thermos/alternatives parameters)]
                                              [id (assoc x ::supply/id id)])
                                            (into {}))
        
        connection-costs               (->> (for [[id x] (:thermos/connection-costs parameters)]
                                              [id (assoc x ::tariff/cc-id id)])
                                            (into {}))
        
        insulation-rules      (:thermos/insulation-rules  parameters)
        alternative-rules     (:thermos/alternative-rules parameters)
        civils-rules          (:thermos/civils-rules parameters)
        connection-cost-rules (:thermos/connection-cost-rules parameters)
        
        ;; construct problem
        problem    (-> defaults/default-document
                       (assoc
                        ::document/candidates       candidates
                        ::document/pipe-costs       pipe-costs
                        ::document/insulation       insulation
                        ::document/alternatives     alternatives
                        ::document/connection-costs connection-costs)

                       ;; apply rules for technologies & requirement
                       (document/map-buildings #(assign-building-options
                                                 % insulation-rules alternative-rules connection-cost-rules))
                       
                       (document/map-paths #(assign-civil-cost % civils-rules))                       
                       (document/map-candidates #(set-requirement % requirement-rules))

                       ;; insert supply points
                       (cond->
                           heat-price
                         (add-supply-points
                          (:thermos/supplies-per-component parameters)
                          (-> (:thermos/cluster-supply-parameters parameters)
                              (assoc ::supply/cost-per-kwh (/ heat-price 100)))))

                       ;; copy other misc parameters.
                       ;; probably worth setting them up not as a blob,
                       ;; although that does mean listing them out here
                       (assoc
                        ::document/objective             :system
                        ::document/consider-alternatives true
                        ::document/consider-insulation   true
                        ::document/maximum-supply-sites  1

                        ::document/npv-rate  (:finance/npv-rate parameters)
                        ::document/npv-term  (:finance/npv-term parameters)
                        
                        ::document/param-gap (:thermos/param-gap parameters)
                        ::document/mip-gap   (:thermos/mip-gap parameters)

                        ::document/maximum-runtime (double
                                                    (/ (:thermos/runtime-limit parameters)
                                                       3600))
                        
                        ::document/maximum-iterations (:thermos/iteration-limit parameters)

                        ::document/capital-costs
                        {:connection  {:recur true :period (:finance/building-hx-lifetime parameters)}
                         :supply      {:recur true :period (:finance/substation-lifetime parameters)}
                         :pipework    {:recur true :period (:finance/distribution-lifetime parameters)}
                         :insulation  {:recur true :period (:finance/insulation-lifetime parameters)}
                         :alternative {:recur true :period (:finance/individual-system-lifetime parameters)}
                         }
                        )
                       )
        _ (print-summary problem)

        ;; crunch crunch run model
        solution   (interop/try-solve problem (fn [& _]))
        
        {buildings :building paths :path} (document/candidates-by-type solution)
        supplies                (filter candidate/supply-in-solution? buildings)
        
        ]

    (output-metadata solution false output-file)
    
    (output problem
            buildings
            paths
            supplies
            output-file
            (::geoio/crs input-features)
            output-geometry
            )))

(defn- fix-supply-choice
  "Restrict supply points to the supply points that got built"
  [problem]
  (document/map-buildings
   problem
   (fn [building]
     (cond-> building
       (and
        (candidate/has-supply? building)
        (not (candidate/supply-in-solution? building)))
       (candidate/forbid-supply!)))))

(defn- round-solution [{:keys [input-file output-file parameters
                               output-geometry]}]
  ;; TODO we could say if there is no heat price cheat and output the input
  (let [parameters (read-edn parameters)
        solution   (read-edn input-file)

        [rounding-decisions rounded-problem]
        (round-groups solution parameters)

        rounded-problem
        (fix-supply-choice rounded-problem)
        
        rounded-solution
        (interop/try-solve rounded-problem (fn [& _]))

        {buildings :building
         paths :path}  (document/candidates-by-type rounded-solution)
        
        supplies (filter candidate/supply-in-solution? buildings)]

    (output-metadata rounded-solution true output-file)

    (write-sqlite
     output-file
     "rounding"
     [["group" :string (comp str :group)]
      ["count" :int :n]
      ["value_in" :double  #(double (get % true 0.0))]
      ["value_out" :double #(double (get % false 0.0))]
      ["decision" :string   (comp name :decision)]]
     rounding-decisions)

    (output rounded-solution
            buildings
            paths
            supplies
            output-file
            "EPSG:27700" ;; urgh no
            output-geometry)))


(comment

  (-main
   "-i" "/home/hinton/p/793-hnzp/hnzp/integration-testing/blobs/problem.edn"
   "-o" "/home/hinton/tmp-hnzp/rounded-outputs.gpkg"
   "-p" "/home/hinton/tmp-hnzp/parameters.edn"
   "--round-and-evaluate"
   "--output-geometry"
   )



  
  )

(ns thermos-cli.zone-cli-io
  (:require [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [thermos-importer.geoio :as geoio]
            [cljts.core :as jts]
            [thermos-specs.document :as document]
            [thermos-specs.solution :as solution]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.path :as path]
            [clojure.edn :as edn])
  
  (:import [java.sql Connection DriverManager SQLException
            PreparedStatement Types]))

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
                      (.setDouble ps ix (double val))
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

      path-capex       #(:principal (::solution/pipe-capex %))

      ;; yuck
      is-mandated?     (fn [building]
                         (cond
                           (and (:mandatable? building)
                                (::solution/connected building))
                           "required"

                           (= (::candidate/inclusion building) :forbidden)
                           "forbidden"

                           :else
                           "optional"))
      ]
  
  (defn output [problem buildings paths supplies
                 output-file crs output-geometry]
    (let [mode (document/mode problem)
          heat-demand-kwh #(candidate/solved-annual-demand % mode)
          peak-demand-kw  #(candidate/solved-peak-demand % mode)

          insulation-kwh  #(- (candidate/annual-demand % mode)
                              (candidate/solved-annual-demand % mode))
          
          civil-cost-name  #(document/civil-cost-name problem (::path/civil-cost-id %))]
      
      (write-sqlite
       output-file
       "buildings"
       (into [["building_id"          :string  building-id]
              ["rounded_building"     :string  :thermos-cli.zone-cli-groups/rounded-building]
              ["rounding_group"       :string  :thermos-cli.zone-cli-groups/rounding-group]
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
              ["mandatable"           :boolean :mandatable?]
              ["mandated"             :string  is-mandated?]]
             
             (when output-geometry
               [["geometry"     :polygon ::candidate/geometry]
                ["conns"        :string  #(string/join "," (::candidate/connections %))]
                ]))
       
       buildings
       :crs crs)

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
        ["start"        :string      ::path/start]
        ["end"          :string      ::path/end]
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

(defn output-metadata [solution rounded output-file]
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

(defn read-edn [file]
  (with-open [r (java.io.PushbackReader. (io/reader (io/as-file file)))]
    (edn/read {:readers {'geojson jts/json->geom}} r)))

(defn write-edn [solution file]
  (with-open [w (io/writer (io/as-file file))]
    (binding [*out* w] (prn solution))))

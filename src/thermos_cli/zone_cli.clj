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
            [mount.core :as mount]
            [thermos-cli.zone-cli-rules :as rules]
            [thermos-cli.zone-cli-groups :as groups]
            [thermos-cli.zone-cli-io :as zone-io])
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
   [nil "--edn-output-file FILE"
    "An EDN output file, that can go direct into --round-and-evaluate"
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

(defn- usage-message [summary] (str "Usage:\n" summary))
(defn- error-message [errors]  (str "Invalid arguments:\n" (string/join \newline errors)))

(defmethod print-method org.locationtech.jts.geom.Geometry
  [this ^java.io.Writer w]
  (.write w "#geojson ")
  (print-method (jts/geom->json this) w))

(declare run-with run-optimiser --main)

(defn -main [& arguments]
  (System/exit (let [ev (--main arguments)]
                 (if (integer? ev) ev 0))))

(defn --main
  "This is for interactive use, so we don't call System/exit which is
  a bit brutal. It's a shame java does public static void main(...) so
  you can't just return an int like a normal sane program."
  [arguments]
  (mount/start-with {#'thermos-backend.config/config {}})
  (let [{:keys [options arguments summary errors]}
        (parse-opts arguments options)

        runtime (-> (into {} (System/getenv))
                    (get "RUNTIME" "3600"))

        runtime (or (try
                      (Integer/parseInt runtime)
                      (catch NumberFormatException nfe
                        (log/warnf "Invalid RUNTIME value %s, using 3600s" runtime)))
                    3600)
        
        options (assoc options :runtime runtime)
        ]

    (binding [*out* *err*]
      (cond
        (:help options)
        (do (println (usage-message summary)) 0)

        (seq errors)
        (do (println (error-message errors)) 1)
        
        (not (empty? arguments))
        (do (println (format "Unexpected arguments: %s" arguments)) 2)
        
        :else
        (run-with options)))))

(defn- run-with [{:keys [input-file output-file parameters heat-price
                         edn-output-file
                         round-and-evaluate
                         output-geometry]
                  :as options}]
  {:pre [(and input-file parameters output-file)]}
  (when (.exists output-file)
    (io/delete-file output-file))

  (when (and edn-output-file (.exists edn-output-file))
    (io/delete-file edn-output-file))

  (let [result
        (if round-and-evaluate
           (groups/round-solution options)
           (run-optimiser options))
        
        state
        (::solution/state result)]
    (cond
      (= :time-limit state)
      (do (println "Hit time limit") 100)

      (= :infeasible state)
      (do (println "Problem unexpectedly infeasible") 101)

      (or (= :killed state)
          (and (= :error state) (= 137 (::solution/error result))))
      (do (println "Optimiser killed") 102)

      (= :empty-problem state)
      (do (println "Problem was empty, but this is OK") 0)
      
      (not (solution/exists? result))
      (do (println "Solution does not exist:" state) 103)

      :else
      (do (println "Finished") 0))))

(defn- line? [feature]
  "is the feature a line feature"
  (-> feature ::geoio/type #{:line-string :multi-line-string} boolean))

(defn- noder-options [parameters]
  {:shortest-face  3.0
   :snap-tolerance 0.1
   :trim-paths     true
   :transfer-field :path-segment})

(defn- make-candidate-path [p]
  (merge
   (json/read-str (get p "meta" "{}"))
   (dissoc p "meta")
   {::candidate/type      :path
    ::candidate/inclusion :optional
    :connector            (:connector p)
    }))

(defn- make-candidate-building [b]
  (merge
   (json/read-str (get b "meta" "{}"))
   (dissoc b "meta")
   {::candidate/type        :building
    ::candidate/inclusion   :optional
    
    ::demand/kwh              (get b "heat_demand_kwh" 0)
    ::demand/kwp              (get b "peak_demand_kw" 0)
    ::demand/connection-count (or (get b "num_customers") 1)
    :height                   (get b "height") ;; for lidar/add-other-attributes
    }))

(defn- set-mandatable [candidate mandation-rule]
  (cond-> candidate
    (rules/matches-rule? candidate mandation-rule)
    (assoc :mandatable? true)))

(defn- set-infill [candidate]
  (cond-> candidate
    (not (:mandatable? candidate))
    (assoc ::demand/infill-groups #{0})))

(defn add-supply-points
  "Modify `instance` to have `n` supply points with the given
  `supply-parameters` in each of its connected components" 
  [instance n supply-parameters]
  (select-top-n-supplies
   instance
   supply-parameters
   n
   (not (contains? supply-parameters ::supply/capacity-kwp))
   true))

(defn- print-summary [problem]
  (let [alternatives (::document/alternatives problem)
        insulation   (::document/measures problem)
        civils       (:civils (::document/pipe-costs problem))
        con-cost     (::document/connection-costs problem)
        candidates   (vals (::document/candidates problem))
        infill-range (::document/infill-targets problem)
        ]

    (println "Mandatable: "
             (frequencies (map (juxt ::candidate/type :mandatable?) candidates)))
    
    (println "Infill:" infill-range
             (frequencies (mapcat ::demand/infill-groups candidates)))
    
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
              (keep (comp ::tariff/name con-cost ::tariff/cc-id) candidates)))))

(defn- geoio-features->candidates
  "in is a geoio features map, so has ::geoio/crs and ::geoio/features
  and each ::geoio/features has ::geoio/geometry and ::geoio/type

  returns a geoio features map but where the other keys on features
  are what thermos needs for candidates."
  [in]
  (assoc in ::geoio/features
         (for [f (::geoio/features in)]
           (merge
            f
            (if (line? f)
              (make-candidate-path f)
              (make-candidate-building f))))))


(defn- is-unheated-building? [f]
  (and (candidate/is-building? f)
       (or (zero? (::demand/kwh f 0))
           (zero? (::demand/kwp f 0)))))

(defn- remove-unheated-buildings [features]
  (update features ::geoio/features #(remove is-unheated-building? %)))

(defn- remove-all-paths [features]
  (update features ::geoio/features #(remove candidate/is-path? %)))

(defn- path-forbidden-by? [rules feature]
  (and (candidate/is-path? feature)
       (let [[match requirement] (rules/matching-rule feature rules)]
         (= requirement :forbidden))))

(defn- remove-forbidden-paths [features rules]
  (update features ::geoio/features
          #(let [n (count %)
                 out (remove (partial path-forbidden-by? rules) %)
                 n' (count out)]
             (println (- n n') "paths removed by rules")
             out)))

(defn- node-and-connect [features parameters]
  (let [;; features (geoio/reproject features "EPSG:4326")

        [paths buildings] (noder/node-connect features (noder-options parameters))
        ;; In node-connect in noder.clj, we have associated :path-segment to
        ;; each path, which is a unique int for each noded segment of input
        ;; road. This will be propagated onto connectors as well according
        ;; to the segment they connect to.

        ;; We use this to put "connects_to" onto paths and buildings as well
        ;; which is the toid of the original road they connect to (a bit
        ;; less specific than the :path-segment value)
        road-segment-to-road-toid
        (reduce
         (fn [out path]
           (let [segment (:path-segment path)
                 toid    (get path "id")]
             (cond-> out
               (and segment toid)
               (assoc segment toid))))
         {}
         paths)
        ]
    (assoc features
           ::geoio/features
           (vec (concat
                 (for [path paths]
                   (assoc (cond-> path (:connector path) (make-candidate-path))
                          ;; connectors need processing still
                          
                          ::path/length (::spatial/length path)
                          ::path/start  (::geoio/id (::spatial/start-node path))
                          ::path/end    (::geoio/id (::spatial/end-node path))

                          "connects_to" (get road-segment-to-road-toid (:path-segment path))))
                 (for [building buildings]
                   (let [segment-toid (get road-segment-to-road-toid
                                           (:path-segment building))]
                     (assoc building
                            ::candidate/connections (::spatial/connects-to-node building)
                            
                            "connects_to" segment-toid
                            ;; path-id is a special group-by field
                            :path-id      segment-toid))))))))

(defn- add-building-dimensions [features]
  (let [[buildings paths]
        ((juxt filter remove) candidate/is-building? (::geoio/features features))

        buildings
        (->> (lidar/add-other-attributes (assoc features ::geoio/features buildings))
             (::geoio/features)
             (map importer/add-areas)
             (map (fn [b]
                    (assoc b
                           ::candidate/wall-area   (:wall-area b)
                           ::candidate/roof-area   (:roof-area b)
                           ::candidate/ground-area (:ground-area b)))))]
    
    (assoc features ::geoio/features (concat paths buildings))))

(defn- remove-duplicate-features [features]
  (let [n (count (::geoio/features features))
        features (update features ::geoio/features set)]
    (println (- n (count (::geoio/features features))) "fully duplicated features")
    features))

(defn- add-candidate-ids [features]
  (update features ::geoio/features
          #(for [f %]
             (assoc f
                    ::candidate/geometry  (::geoio/geometry f)
                    ::candidate/id        (::geoio/id f)))))

(defn- run-optimiser [{:keys [input-file output-file parameters heat-price
                              edn-output-file
                              runtime
                              output-geometry]
                       :as options}]
  (let [parameters (zone-io/read-edn parameters)
        requirement-rules (:thermos/requirement-rules parameters)
        mandation-rule    (:thermos/mandation-rule    parameters)

        input-features (geoio/read-from input-file :key-transform identity)
        
        candidates
        (-> input-features
            (remove-duplicate-features)
            (geoio-features->candidates)
            (remove-unheated-buildings)
            (cond-> (not heat-price) (remove-all-paths)
                    heat-price       (remove-forbidden-paths requirement-rules))
            (add-building-dimensions)
            (node-and-connect parameters)
            (add-candidate-ids)
            (::geoio/features)
            (thermos-util/assoc-by ::candidate/id))

        
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
        infill-range          (:thermos/infill-range parameters false)
        group-fields          (:thermos/group-buildings-by parameters)
        
        ;; construct problem
        problem    (-> defaults/default-document
                       (assoc
                        ::document/candidates       candidates
                        ::document/pipe-costs       pipe-costs
                        ::document/insulation       insulation
                        ::document/alternatives     alternatives
                        ::document/connection-costs connection-costs)

                       ;; apply rules for technologies & requirement etc
                       (document/map-buildings (fn apply-building-rules [b]
                                                 (-> b
                                                     (set-mandatable mandation-rule)
                                                     (cond-> (and infill-range heat-price) (set-infill))
                                                     (groups/set-optimiser-group group-fields)
                                                     (rules/assign-matching-value insulation-rules true ::demand/insulation)
                                                     (rules/assign-matching-value alternative-rules true ::demand/alternatives)
                                                     (rules/assign-matching-value connection-cost-rules false ::tariff/cc-id))))
                       
                       (document/map-paths (fn apply-path-rules [p]
                                             (rules/assign-matching-value p civils-rules false ::path/civil-cost-id)))

                       ;; requirement rules need more thought
                       ;; if we forbid a building we effectively delete it for the optimiser
                       ;; what we want is instead to forbid it from network but include it in the problem
                       (document/map-candidates (fn apply-requirement-rules-and-infill [c]
                                                  (-> (assoc c ::candidate/inclusion :optional)
                                                      (rules/assign-matching-value requirement-rules false ::candidate/inclusion))))


                       
                       ;; insert supply points
                       (cond-> heat-price
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

                        ::document/npv-rate  (:finance/npv-rate parameters)
                        ::document/npv-term  (:finance/npv-term parameters)
                        
                        ::document/param-gap (:thermos/param-gap parameters)
                        ::document/mip-gap   (:thermos/mip-gap parameters)

                        ::document/should-be-feasible true
                        ::document/infill-targets (and infill-range {0 infill-range})

                        ::document/maximum-runtime (double (/ runtime 3600))
                        
                        ::document/maximum-iterations (:thermos/iteration-limit parameters)

                        ::document/capital-costs
                        {:connection  {:recur true :period (:finance/building-hx-lifetime parameters)}
                         :supply      {:recur true :period (:finance/substation-lifetime parameters)}
                         :pipework    {:recur true :period (:finance/distribution-lifetime parameters)}
                         :insulation  {:recur true :period (:finance/insulation-lifetime parameters)}
                         :alternative {:recur true :period (:finance/individual-system-lifetime parameters)}
                         }
                        ))
        
        _ (print-summary problem)
        
        ;; crunch crunch run model
        solution   (interop/try-solve problem (fn [& _]))
        
        {buildings :building paths :path} (document/candidates-by-type solution)
        supplies                (filter candidate/supply-in-solution? buildings)
        
        ]
    
    (zone-io/output-metadata solution false output-file)
    
    (zone-io/output problem
                    buildings
                    paths
                    supplies
                    output-file
                    (::geoio/crs input-features)
                    output-geometry)

    (when edn-output-file
      (zone-io/write-edn solution edn-output-file))
    
    solution))

(comment
  (--main
   ["-i" "/home/hinton/infeasible/map.gpkg"
    "-o" "/home/hinton/infeasible/out.gpkg"
    "--edn-output-file" "/home/hinton/infeasible/out.edn"
    "-p" "/home/hinton/infeasible/parameters.edn"
    "--output-geometry"
    "--heat-price" "14.0"]
   )

  (--main
   ["-i" "/home/hinton/infeasible/out.edn"
    "-o" "/home/hinton/infeasible/out-r.gpkg"
    "--round-and-evaluate"
    "-p" "/home/hinton/infeasible/parameters.edn"
    "--output-geometry"
    ;; "--heat-price"
    ;; "14.0"
    ]
   )

  
  
  )


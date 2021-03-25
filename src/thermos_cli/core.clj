(ns thermos-cli.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [thermos-importer.geoio :as geoio]
            [thermos-importer.spatial :as spatial]
            [cljts.core :as jts]
            [clojure.tools.logging :as log]
            [clojure.string :as string]
            
            [thermos-backend.importer.process :as importer]
            [thermos-backend.solver.interop :as interop]
            [thermos-backend.spreadsheet.core :as spreadsheet]
            [thermos-util :refer [as-double as-boolean as-integer assoc-by]]

            [thermos-importer.lidar :as lidar]
            
            [thermos-specs.document :as document]
            [thermos-specs.defaults :as defaults]

            [thermos-specs.path :as path]
            [thermos-specs.measure :as measure]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.supply :as supply]
            [thermos-specs.tariff :as tariff]
            [thermos-specs.solution :as solution]
            [mount.core :as mount]
            [com.rpl.specter :as S]
            
            [loom.alg :as graph-alg]
            [thermos-util.pipes :as pipes]
            [thermos-cli.output :as output]
            [clojure.set :as set])

  (:gen-class))

;; THERMOS CLI tools for Net Zero Analysis

(defn- conj-arg [m k v]
  (update m k conj v))

(defn- setwise-keyword-option [short long doc values]
  [short long (str doc ": " (string/join " | " (map name values)))
   :parse-fn keyword
   :validate [(set values) (str "Must be one of " (string/join ", " (map name values)))]])

(def options
  [[nil "--problem-name NAME" "Name of problem - this will be put in some outputs"]
   ["-i" "--base FILES" "The problem to start with - this may contain geometry already.
An efficient way to use the tool is to put back in a file produced by a previous -o output."
    :assoc-fn conj-arg]
   
   ["-o" "--output FILE*" "The problem will be written out here. Format determined by file extension. Can be repeated.
If the file is a tsv file then if the name contains 'pipe' the output will be about pipes, otherwise buildings.
If the file name contains 'summary' summary data will be written.
TSV columns for pipes & buildings:
- problem : --problem-name
- id : --id-field
- lon,lat : epsg4326 centroid
- system : heating system (buildings)
- kwh, kwp : kwh (after ins.) and kwp (buildings)
- insulation: kwh/yr avoided by insulation (buildings) 
- capex : heating system capex (heat exch. for net, buildings)
- opex : heating system annual whole-system cost (buildings, i.e. no network revenues)
- revenue : heating system annual revenue to network (buildings)
- count : n connections
- skwp : supply capacity if supply
- scapex : system capex if supply
- sopex : system opex if supply
- icapex : insulation capex
- length : pipe length (m)
- diameter : pipe diameter (mm)
- kw : pipe capacity (kw)
- civils : civil cost name (pipes)
- capex : pipe capex
- losses : kwh/yr losses
- diversity : diversity / coincidence factor
"
    :assoc-fn conj-arg]

   [nil "--id-field FIELD" "An ID field to output in TSV files"]

   ;; ["-s"  "--output-summary FILE*" "Solution summary data will be written out here. Format by extension, can be repeated."
   ;;  :assoc-fn conj-arg]

   ["-m" "--map FILE*"
    "Geodata files containing roads or buildings.
These geometries will replace everything in the base scenario (NOT combine with).
If there are roads in there, they will be noded and connected to the buildings.
If there are buildings, they will have demand and peak demand computed, subject to the other fields configured."
    :assoc-fn conj-arg]
   ["-l" "--lidar FILE" "Path to some LIDAR - if map data is given, the LIDAR will be used to put heights on it."
    :assoc-fn conj-arg]
   [nil "--degree-days NUMBER" "Annual heating degree days (relative to 17°) to use for running heat demand model."
    :parse-fn #(Double/parseDouble %)
    :default (* 0.813 2501.0)]
   [nil "--connect-to-connectors" "Allow connecting to connectors."]
   [nil "--shortest-face LENGTH" "When finding face centers, only faces longer than this will be considered."
    :default 3.0
    :parse-fn #(Double/parseDouble %)]
   [nil "--snap-tolerance DIST" "Patch holes in the network of length DIST."
    :default nil
    :parse-fn #(Double/parseDouble %)]
   [nil "--trim-paths" "Remove paths that don't go anywhere."]

   [nil "--ignore-paths FIELD=VALUE*" "Ignore paths where FIELD=VALUE"
    :assoc-fn conj-arg
    :parse-fn #(string/split % #"=")]
   
   [nil "--solve" "Run the network model solver."]
   [nil "--height-field FIELD" "A height field, used in preference to LIDAR."]
   [nil "--supply-field FIELD" "A field which, if true, will be used to select a supply location."]
   [nil "--fallback-height-field FIELD" "A height field, used if LIDAR (and given height) is missing."]
   [nil "--resi-field FIELD" "A field which is used to tell the demand model if a building is residential."]
   [nil "--demand-field FIELD" "A kwh/yr field. If given, this will be used in preference to the demand model."]
   [nil "--confidence-field FIELD" "How confident you are in the demand field (max / use).
If given, then a shape for which this says 'max' will get the maximum of demand modelled and demand-field value.
If it says 'use', then the demand-field value will be used (unless it's not numeric)."]
   [nil "--peak-field FIELD" "A kwp field. If given, this will be used in preference to peak model."]
   [nil "--infer-peak-from-diameter FIELD"
    "If given, the peak will be determined from the diameter in FIELD.
Use in conjunction with --transfer-field to get diameter off a pipe."
    ]
   [nil "--infer-peak-at VAL"
    "When inferring peak from diameter, there is a range from (next lowest dia) + 1kW to (next highest) - 1kW. This value is used to interpolate between them."
    :default 0.5
    :parse-fn #(Double/parseDouble %)
    ]
   
   [nil "--count-field FIELD" "Connection count. Otherwise we assume 1."]
   [nil "--require-all" "Require all buildings be connected to network."]
   [nil "--transfer-field FIELD"
    "Set FIELD on buildings to the value of FIELD on the road the connect to."]
   [nil "--group-field FIELD" "Use FIELD to group buildings connection decisions together."]
  
   [nil "--connector-civil-cost NAME" "Use civil cost with NAME for connectors"]
   [nil "--default-civil-cost NAME" "Set the default civil cost to that with NAME"]
   
   [nil "--spreadsheet FILE"
    "Load parameters from an excel spreadsheet; this will only be useful if you also set up the magic fields."]
   
   [nil "--insulation FILE*"
    "A file containing insulation definitions"
    :assoc-fn conj-arg]
   [nil "--alternatives FILE*"
    "A file containing alternative technologies"
    :assoc-fn conj-arg]
   [nil "--supply FILE*"
    "A file containing supply parameters for supply point. See also --supply-capex etc"]

   [nil "--supply-capex C"
    "Instead of using --supply; add a supply with this fixed cost"
    :parse-fn #(Double/parseDouble %)]
   [nil "--supply-capex-per-kwp C" "Supply cost per kW"
    :parse-fn #(Double/parseDouble %)]
   [nil "--supply-capacity-kw C" "Supply max capacity in kW"
    :parse-fn #(Double/parseDouble %)]
   [nil "--supply-cents-per-kwh C" "Supply heat cost per cent/penny"
    :parse-fn #(Double/parseDouble %)]
   [nil "--supply-opex-per-kwp C" "Supply opex per kW per annum"
    :parse-fn #(Double/parseDouble %)]
   
   [nil "--pipe-costs FILE"
    "A file containing pipe cost parameters. Complex structure for this means not mergeable."]
   [nil "--tariffs FILE*"
    "A file containing tariff definitions"
    :assoc-fn conj-arg]

   [nil "--top-n-supplies N" "Number of supplies to introduce into the map by taking the top N demands"
    :default 1
    :parse-fn #(Integer/parseInt %)]

   [nil "--fit-supply-capacity"
    "Make sure the supply added to each component (with --top-n-supplies) is large enough to meet all the demand."]

   [nil "--max-runtime N" "Max runtime hours"
    :parse-fn #(Double/parseDouble %)]
   [nil "--mip-gap G" "Mip gap %"
    :parse-fn #(/ (Double/parseDouble %) 100.0)]

   [nil "--max-iters N" "Stop after N tries"
    :parse-fn #(Integer/parseInt %)]

   [nil "--param-gap X%" "Stop if parameter fixing has less than X% effect"
    :parse-fn #(/ (Double/parseDouble %) 100.0)]
   
   [nil "--set '[A B C V]'"
    "Set the value at path A B C to value V"
    :parse-fn #(edn/read-string %)
    :assoc-fn conj-arg]

   [nil "--retry" "If the optimiser runs but does not find an answer in the time, retry with different options.
The different options are those supplied after --retry, so mostly you can use this to bump up the runtime or similar."
    :assoc-fn (fn [m k _] (assoc m :retry m))
    ;; so :retry in the options is the pre-retry options.
    ]

   (setwise-keyword-option nil "--scip-emphasis X" "Overall scip emphasis" lp.scip/emphasis-values)
   (setwise-keyword-option nil "--scip-presolving-emphasis X" "scip presolver emphasis" lp.scip/presolving-emphasis-values)
   (setwise-keyword-option nil "--scip-heuristics-emphasis X" "scip branch/bound heuristics emphasis" lp.scip/heuristics-emphasis-values)

   ["-h" "--help" "me Obi-Wan Kenobi - You're my only hope."]])

(defn read-edn [file]
  (when file
    (with-open [r (java.io.PushbackReader. (io/reader file))]
      (edn/read
       {:readers {'geojson jts/json->geom}}
       r))))

(defn- concat-edn [files]
  (when files
    (doall
     (apply concat (map read-edn files)))))

(defn- line? [feature]
  (-> feature ::geoio/type #{:line-string :multi-line-string} boolean))

(defn- node-connect
  "Given a set of shapes, do the noding and connecting dance"
  [{crs ::geoio/crs features ::geoio/features}

   {:keys [shortest-face
           snap-tolerance trim-paths
           transfer-field]}
   ]
  (when (seq features)
    (let [{lines true not-lines false}
          (group-by line? features)

          lines (spatial/node-paths lines :snap-tolerance snap-tolerance :crs crs)

          [buildings roads]
          (if lines
            (spatial/add-connections
             crs not-lines lines
             :copy-field (and transfer-field [transfer-field transfer-field])
             :shortest-face-length shortest-face
             :connect-to-connectors false)
            [not-lines nil])]

      [(cond-> roads
         trim-paths
         (spatial/trim-dangling-paths buildings))
       buildings])))

(defn- generate-demands [buildings

                         {lidar :lidar
                          degree-days :degree-days

                          resi-field :resi-field
                          height-field :height-field
                          fallback-height-field :fallback-height-field
                          peak-field :peak-field
                          demand-field :demand-field
                          count-field :count-field
                          confidence-field :confidence-field}
                         ]
  
  (when (seq buildings)
    (let [sqrt-degree-days (Math/sqrt degree-days)
          lidar-index (->> lidar
                           (map io/file)
                           (mapcat file-seq)
                           (filter #(and (.isFile %)
                                         (let [name (.getName %)]
                                           (or (.endsWith name ".tif")
                                               (.endsWith name ".tiff")))))
                           (lidar/rasters->index))

          buildings (lidar/add-lidar-to-shapes buildings lidar-index)

          buildings (::geoio/features buildings)
          ]
      ;; run the model for each building
      (for [b buildings]
        (let [is-resi (as-boolean (or
                                   (not resi-field)
                                   (and resi-field
                                        (get b resi-field))))
              height (and height-field
                          (as-double (get b height-field)))

              fallback-height (and fallback-height-field
                                   (as-double (get b fallback-height-field)))
              
              peak (and peak-field
                        (as-double (get b peak-field)))

              demand (and demand-field
                          (as-double (get b demand-field)))

              confidence (and confidence-field
                              (#{:use :max} (get b confidence-field)))
              
              count (or (and count-field
                             (as-integer (get b count-field)))
                        1)]
          
          (-> b
              (assoc :residential is-resi
                     :connection-count count)
              (cond->
                height
                (assoc :height height)

                fallback-height
                (assoc :fallback-height fallback-height)

                demand
                (assoc :annual-demand demand)

                (and confidence demand)
                (assoc :use-annual-demand (keyword confidence)))

              (importer/produce-heat-demand sqrt-degree-days)
              (as-> x
                  (assoc x :peak-demand
                         (or peak
                             (importer/run-peak-model
                              (:annual-demand x)))))))))))

(defn- match
  "Match ITEM, a map, against OPTIONS, a list of things with a :rule in them.
  A :rule is a tuple going [field pattern], so when we (get field
  item) it matches pattern (a regex literal or set of values)"
  [item options & {:keys [match] :or {match :thermos-cli/rule}}]
  
  (and item
       (filter
        (fn matches? [option]
          (let [rule (get option match)]
            (cond
              (= true rule)
              option

              (vector? rule)
              (let [[field pattern] rule
                    field-value (get item field)]
                (cond
                  (string? pattern)
                  (re-find (re-pattern pattern) (str field-value))
                  
                  (set? pattern)
                  (contains? pattern field-value)

                  :else false)
                ))))
        options)))

(defn- add-civils
  "Use `match` to join pipe-costs to paths.
  The rule structure for paths is that the pipe-costs blob contains
  :thermos-cli/rule, which is a series of vectors, that look like

  [civil-cost-id RULE]

  where RULE is like what `match` takes.
  "
  [path pipe-costs]
  (let [civils (:civils pipe-costs)
        rules  (:thermos-cli/rule pipe-costs)]
    (let [civil (first (match path rules :match second))]
      (cond-> path
        civil
        (assoc ::path/civil-cost-id (first civil))))))

(defn- add-insulation [building insulation]
  (let [insulation (match building insulation)
        insulation (set (map ::measure/id insulation))]
    (assoc building ::demand/insulation insulation)))

(defn- add-alternatives [building alternatives]
  (let [alts (match building alternatives)
        counter (::supply/id (first (match building alternatives :match :thermos-cli/counterfactual-rule)))
        alts (set (map ::supply/id alts))
        alts (cond-> alts counter (disj counter))]
    (cond-> building
      (seq alts)
      (assoc ::demand/alternatives alts)

      counter
      (assoc ::demand/counterfactual counter))))

(defn- add-tariff [building tariffs]
  (let [tariff (::tariff/id (first (match building tariffs)))]
    (cond-> building
      tariff (assoc ::tariff/id tariff))))

(defn- select-top-n-supplies [instance supply top-n fit-supply]
  (let [{buildings :building paths :path}
        (document/candidates-by-type instance)

        graph
        (interop/create-graph buildings paths)

        components
        (graph-alg/connected-components graph)

        candidates (::document/candidates instance)
        
        ranked-building-ids
        (fn [building-ids]
          (let [buildings
                (keep
                 (fn [id]
                   (let [candidate (get candidates id)]
                     (when (candidate/is-building? candidate)
                       {:id (::candidate/id candidate)
                        :kwh (::demand/kwh candidate)
                        :centroid (.getCentroid (::candidate/geometry candidate))})))
                 building-ids)

                buildings
                (for [{id :id here :centroid} buildings]
                  {:id id
                   :value
                   (double
                    (reduce
                     +
                     (for [{kwh :kwh there :centroid} buildings]
                       (/ kwh
                          (+ 50.0  ;; might work?
                             (jts/geodesic-distance
                              (.getCoordinate here)
                              (.getCoordinate there))
                             )))))})
                ]

            (->> buildings
                 (sort-by :value #(compare %2 %1))
                 (keep :id))))


        building-peak (fn [c] (candidate/peak-demand
                               (get candidates c)
                               (document/mode instance)))
        
        ;; for each component compute a tuple
        ;; [some buildings IDs, total kW]
        ;; buildings IDs are where we want to put supplies
        ;; total kW is the total demand in the component, in case fit-supply is true
        components-and-supplies
        (map
         #(vector
           (take top-n (ranked-building-ids %))
           (reduce + 0 (keep building-peak %)))
         components)
        ]

    (doseq [[supplies size] components-and-supplies]
      (log/info "Adding" (count supplies) "to a" size "kW component"))

    (if (and (seq components-and-supplies) supply)
      (reduce
       (fn [instance [supply-ids max-capacity]]
         (let [supply (cond-> supply
                        fit-supply
                        (assoc ::supply/capacity-kwp max-capacity))]
           (document/map-candidates
            instance (fn [candidate] (merge supply candidate))
            supply-ids)))
       instance components-and-supplies)

      instance)))

(defn- select-input-supplies [instance supply supply-field]
  (log/info "Adding supplies based on" supply-field)
  (document/map-buildings
   instance
   (fn [building]
     (if-let [field (as-boolean (get building supply-field))]
       (merge supply building)
       building))))

(defn- select-supply-location [instance options]
  (cond

    (:supply-field options)
    (select-input-supplies instance (:supply options) (:supply-field options))

    (pos? (:top-n-supplies options))
    (select-top-n-supplies instance (:supply options) (:top-n-supplies options) (:fit-supply-capacity options))

    :else ;; NOP
    instance))

(defn- make-candidates [paths buildings]
  (let [paths (for [path paths]
                (-> path
                    (merge {::candidate/id        (::geoio/id path)
                            ::candidate/type      :path
                            ::candidate/subtype   (:subtype path)
                            ::candidate/inclusion :optional
                            ::path/length         (or (::spatial/length path) 0)
                            ::path/start          (::geoio/id (::spatial/start-node path))
                            ::path/end            (::geoio/id (::spatial/end-node path))
                            ::candidate/geometry  (::geoio/geometry path)})))
        buildings (for [building buildings]
                    (-> building
                        (merge {::candidate/id            (::geoio/id building)
                                ::candidate/subtype       (:subtype building)
                                ::candidate/type          :building
                                ::candidate/inclusion     :optional
                                ::candidate/wall-area     (:wall-area building)
                                ::candidate/roof-area     (:roof-area building)
                                ::candidate/ground-area   (:ground-area building)
                                ::candidate/connections   (::spatial/connects-to-node building)
                                ::candidate/geometry      (::geoio/geometry building)
                                
                                ::demand/kwh              (:annual-demand building)
                                ::demand/kwp              (:peak-demand building)
                                ::demand/connection-count (:connection-count building)
                                })))]

    (assoc-by
     (concat paths buildings)
     ::candidate/id)))

(defn- sum-costs [costs]
  (apply merge-with +
         (for [c costs :when c]
           {:present (:present c 0)
            :total (:total c 0)
            :annual (:annual c 0)
            :principal (:principal c 0)})))

(defn- problem-summary
  "Compute some useful summary stats about the given instance."
  [instance name]

  (let [{buildings :building paths :path}
        (group-by ::candidate/type
                  (vals (::document/candidates instance)))

        {network-buildings true
         individual-buildings false}
        (group-by (comp boolean candidate/is-connected?)
                  buildings)

        supplies (filter ::solution/capacity-kw buildings)

        network-paths
        (filter candidate/is-connected? paths)
        ]
    {:name name

     :problem
     {:building-count (count buildings)
      :address-count  (reduce + (keep ::demand/connection-count buildings))
      :path-count     (count paths)
      :kwh            (reduce + (keep ::demand/kwh buildings))
      :kwp            (reduce + (keep ::demand/kwp buildings))
      :path-length   (reduce + (keep ::path/length paths))

      :components          (count (keep ::supply/capacity-kwp buildings))
      }

     :solution
     {:runtime             (::solution/runtime instance)
      :gap                 (::solution/gap instance)
      :objective           (::solution/objective instance) }
     
     :network
     
     (let [heat-output (reduce + (keep ::solution/kwh network-buildings))
           heat-input  (->> supplies
                            (keep ::solution/output-kwh)
                            (reduce +))
           ]

       {:building-count    (count network-buildings)
        :address-count     (reduce + (keep ::demand/connection-count network-buildings))

        :undiverse-kwp     (reduce + (keep ::demand/kwp network-buildings))
        :kwp               (reduce + (keep ::solution/capacity-kw supplies))
        :length            (reduce + (keep ::path/length network-paths))

        :kwh-output        heat-output
        :kwh-input         heat-input
        :kwh-lost          (- heat-input heat-output)

        :supply-capex (sum-costs (keep ::solution/supply-capex supplies))
        :heat-cost (sum-costs (keep ::solution/heat-cost supplies))
        :supply-opex (sum-costs (keep ::solution/supply-opex supplies))
        :path-capex (sum-costs (keep ::solution/pipe-capex network-paths))
        :connection-capex (sum-costs (keep ::solution/connection-capex buildings))

        :capex
        (sum-costs (concat
                    (keep ::solution/connection-capex buildings)
                    (keep ::solution/supply-capex buildings)
                    (keep ::solution/pipe-capex network-paths)))

        :opex
        (sum-costs
         (concat
          (keep ::solution/supply-opex buildings)
          (keep ::solution/heat-cost buildings)))
        
        })

     :insulation
     (->>
      (for [[name installed]
            (->> buildings
                 (mapcat ::solution/insulation)
                 (group-by ::measure/name))]
        [name
         {:kwh-avoided    (reduce + (keep :kwh installed))
          :building-count (count installed)
          :area           (reduce + (keep :area installed))
          :capex          (sum-costs installed)}])
      (into {}))
     
     
     :alternatives
     (->>
      (for [[name alts]
            (group-by (comp ::supply/name ::solution/alternative)
                      individual-buildings)]
        [(or name "Nothing at all")
         {:kwh-output (reduce + (keep ::solution/kwh alts))
          :kwp        (reduce + (keep ::demand/kwp alts))
          :building-count (count alts)
          :address-count  (reduce + (keep ::demand/connection-count alts))
          :capex (sum-costs (keep (comp :capex ::solution/alternative) alts))
          :heat-cost (sum-costs (keep (comp :heat-cost ::solution/alternative) alts))
          :opex (sum-costs (keep (comp :opex ::solution/alternative) alts))}])
      (into {}))}))

(defn- set-values [instance things-to-set]
  (reduce
   (fn [i s]
     (let [path (take (dec (count s)) s)
           value (last s)]
       (log/info "change" path "from"
                 (get-in i path)
                 "to"
                 value)
       (assoc-in i path value)))
   instance
   things-to-set))

(defn- require-all-buildings [instance]
  (document/map-buildings
   instance
   (fn [building] (assoc building ::candidate/inclusion :required))))

(defn- set-groups [instance group-field]
  (document/map-buildings
   instance
   (fn [building]
     (if-let [g (get building group-field)]
       (assoc building ::demand/group g)
       building))))

(defn- infer-peak-demand [instance diameter-field interpolate-to]
  (let [curve      (pipes/curves instance)]
    ;; to do reverse lookup we need pipe functions
    
    (document/map-buildings
     instance
     (fn [building]
       (if-let [dia (as-double (get building diameter-field))]
         ;; choose a demand that gets us this diameter
         (let [[kwmin kwmax] (pipes/dia->kw-range curve dia)]
           (assoc building ::demand/kwp (+ kwmin (* interpolate-to (- kwmax kwmin)))))
         ;; do nothing
         building)))))

(defn- set-default-civil-cost [document civil-cost-name]
  (let [pipe-costs (::document/pipe-costs document)
        civils     (:civils pipe-costs)
        by-name    (set/map-invert civils)
        default-id (get by-name civil-cost-name)
        ]
    (when-not default-id
      (log/warn "No civil cost is defined for default" civil-cost-name))
    (cond-> document
      default-id
      (assoc-in [::document/pipe-costs :default-civils] default-id))))

(defn- set-connector-civil-cost [document civil-cost-name]
  (let [pipe-costs (::document/pipe-costs document)
        civils     (:civils pipe-costs)
        by-name    (set/map-invert civils)
        cost-id (get by-name civil-cost-name)
        ]
    (when-not cost-id
      (log/warn "No civil cost is defined for" civil-cost-name))
    (cond-> document
      cost-id
      (->> (S/setval [::document/candidates S/MAP-VALS (S/pred :connector) ::path/civil-cost-id] cost-id)))))

(defn --main [options]
  (mount/start-with {#'thermos-backend.config/config {}})
  (let [output-paths         (:output options)
        ;; summary-output-paths (:summary-output options)
        
        geodata           (when (seq (:map options))
                            (geoio/read-from-multiple (:map options)
                                                      :key-transform identity))

        geodata           (cond-> geodata

                            (seq (:ignore-paths options))
                            (update ::geoio/features
                                    (let [ignore-fields (:ignore-paths options)
                                          ignored? (fn [x]
                                                     (and (line? x)
                                                          (some identity (for [[f v] ignore-fields] (= (get x f) v)))))]
                                      (fn [features]
                                        (let [out (remove ignored? features)]
                                          (log/info
                                           "Ignored" (- (count features) (count out))
                                           "paths of" (count features) "geoms")
                                          out)
                                        ))))
        
        [paths buildings] (node-connect geodata options)
        
        
        buildings         (when (seq buildings)
                            (-> {::geoio/features buildings
                                 ::geoio/crs (::geoio/crs geodata)}

                                (generate-demands    options)
                                
                                ;; areas for measures to work on
                                (->> (map importer/add-areas))))
        
        instance
        (apply merge
               defaults/default-document

               (when-let [spreadsheet (:spreadsheet options)]
                 (log/info "Reading settings from" spreadsheet)
                 (let [in (spreadsheet/from-spreadsheet spreadsheet)]
                   (when (:import/errors in)
                     (throw (ex-info "Spreadsheet not valid" in)))
                   in))
               
               (when-let [base
                          (seq
                           (filter
                            #(let [e (.exists (io/file %))]
                               (when-not e
                                 (log/warn "--base" % "doesn't exist"))
                               e)
                            (:base options)))]
                 (doall (map read-edn (reverse base)))))
        
        saying            (fn [x s] (log/info s) x)

        instance          (cond-> instance
                            (seq (:tariffs options))
                            (-> (saying "Replace tariffs")
                                (assoc ::document/tariffs (assoc-by (:tariffs options) ::tariff/id))
                                (document/map-buildings (let [tariffs (:tariffs options)] #(add-tariff % tariffs))))
                            (seq (:pipe-costs options))
                            (-> (saying "Replace pipe costs")
                                (assoc ::document/pipe-costs (:pipe-costs options))
                                (document/map-paths
                                 (let [pipe-costs (:pipe-costs options)]
                                   #(add-civils % pipe-costs))))

                            (seq (:insulation options))
                            (-> (saying "Replace insulation")
                                (assoc  ::document/insulation   (assoc-by (:insulation options) ::measure/id))
                                (document/map-buildings (let [insulation (:insulation options)] #(add-insulation % insulation))))
                            
                            (seq (:alternatives options))
                            (-> (saying "Replace alts")
                                (assoc  ::document/alternatives (assoc-by (:alternatives options) ::supply/id))
                                (document/map-buildings (let [alternatives (:alternatives options)] #(add-alternatives % alternatives))))

                            (or (seq paths) (seq buildings))
                            (-> (saying "Replace geometry")
                                (assoc  ::document/candidates   (make-candidates paths buildings)))

                            (:default-civil-cost options)
                            (-> (saying "Set default civil cost")
                                (set-default-civil-cost (:default-civil-cost options)))

                            (:connector-civil-cost options)
                            (-> (saying "Set connector civil cost")
                                (set-connector-civil-cost (:connector-civil-cost options)))
                            
                            (seq (:supply options))
                            (-> (saying "Add supplies")
                                (select-supply-location options))

                            (seq (:set options))
                            (set-values (:set options))

                            (:max-runtime options)
                            (assoc :thermos-specs.document/maximum-runtime
                                   (:max-runtime options))

                            (:mip-gap options)
                            (assoc :thermos-specs.document/mip-gap
                                   (:mip-gap options))

                            (:max-iters options)
                            (assoc :thermos-specs.document/maximum-iterations
                                   (:max-iters options))

                            (:param-gap options)
                            (assoc :thermos-specs.document/param-gap
                                   (:param-gap options))
                            
                            (:require-all options)
                            (-> (saying "Requiring all buildings")
                                (require-all-buildings))

                            (:group-field options)
                            (-> (saying "Set group field")
                                (set-groups (:group-field options)))

                            (:infer-peak-from-diameter options)
                            (-> (saying "Infer peak demand field")
                                (infer-peak-demand (:infer-peak-from-diameter options)
                                                   (:infer-peak-at options 0.5)))
                            
                            (:solve options)
                            (-> (saying "Solve")
                                (as-> x 
                                    (binding [lp.scip/*default-solver-arguments*
                                              (cond-> lp.scip/*default-solver-arguments*
                                                (:scip-emphasis options)
                                                (assoc :emphasis (:scip-emphasis options))

                                                (:scip-heuristics-emphasis options)
                                                (assoc :heuristics-emphasis (:scip-heuristics-emphasis options))
                                                
                                                (:scip-presolving-emphasis options)
                                                (assoc :presolving-emphasis (:scip-presolving-emphasis options)))]
                                      (interop/solve x)))))]
    
    
    (binding [output/*problem-id* (:problem-name options)
              output/*id-field*   (:id-field options)]
      (doseq [output-path output-paths]
        (log/info "Saving state to" output-path)
        (output/save-state instance output-path)))
    
    (mount/stop)

    (cond
      (not (:solve options)) :not-run

      (solution/exists? instance) :solved

      ;; if we did solve, but there is no solution, and time-limit is
      ;; the status:
      (= :time-limit (::solution/state instance))
      :timeout
      
      :else :unknown)))

(defn- generate-ids [things id]
  (map-indexed (fn [i t] (assoc t id i)) things))

(defn- construct-supply-from-args
  "Deal with --supply-X arguments"
  [options]
  (cond-> options
    (:supply-capex options)
    (assoc-in [:supply ::supply/fixed-cost]
              (:supply-capex options))
    (:supply-capex-per-kwp options)
    (assoc-in [:supply ::supply/capex-per-kwp]
              (:supply-capex-per-kwp options))
    (:supply-capacity-kw options)
    (assoc-in [:supply ::supply/capacity-kwp]
              (:supply-capacity-kw options))
    (:supply-cents-per-kwh options)
    (assoc-in [:supply ::supply/cost-per-kwh]
              (/ (:supply-cents-per-kwh options) 100.0))
    (:supply-opex-per-kwp options)
    (assoc-in [:supply ::supply/opex-per-kwp]
              (:supply-opex-per-kwp options))))

(defn- finalize-options [options]
  (-> options
      (update :insulation   concat-edn)
      (update :alternatives concat-edn)
      (update :tariffs      concat-edn)
      (update :pipe-costs   read-edn)
      (update :supply       read-edn)
      
      (update :insulation   generate-ids ::measure/id)
      (update :alternatives generate-ids ::supply/id)
      (update :tariffs      generate-ids ::tariff/id)

      (construct-supply-from-args)))

(defn -main [& args]
  (let [retries (loop [out []
                       cur []
                       args args]
                  (cond (empty? args)
                        (cond-> out (or (empty? out) (seq cur)) (conj cur))

                        (= "--retry" (first args))
                        (recur (conj out cur) [] (rest args))

                        :else
                        (recur out (conj cur (first args)) (rest args))))

        parses (mapv #(parse-opts % options) retries)
        errors (mapcat :errors parses)
        ]

    (cond
      (some :help (map :options parses))
      (binding [*out* *err*]
        (println "SUMMARY:")
        (println (:summary (first parses))))

      (seq errors)
      (binding [*out* *err*]
        (println "INVALID ARGS: ")
        (doseq [e (set errors)] (println e)))
      
      :else
      (loop [optionses (mapv (comp finalize-options :options) parses)]
        (let [[options & optionses] optionses
              outcome (--main options)
              ]
          (when (and (= outcome :timeout) (seq optionses))
            (log/info "No solution, retry with next options")
            (recur optionses)))))))

(comment
  (binding [lp.io/*keep-temp-dir* true]
   
   (-main
    "-m"               "/home/hinton/p/738-cddp/cluster-runner/c_500_5=306.gpkg"
    "--spreadsheet"    "/home/hinton/p/738-cddp/cluster-runner/cddp-thermos-parameters-current.xlsx"
    "--supply"         "/home/hinton/p/738-cddp/cluster-runner/5p.edn"
    "--demand-field" "annual_demand" "--peak-field" "peak_demand" "--count-field" "connection_count"
    "--ignore-paths" "hierarchy=path"
    "--default-civil-cost" "soft"
    "-o" "/home/hinton/tmp/blah-pipes.tsv"
    "-o" "/home/hinton/tmp/blah.json"
    )
   )

  
  )

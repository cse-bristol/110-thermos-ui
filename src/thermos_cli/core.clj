(ns thermos-cli.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [thermos-importer.geoio :as geoio]
            [thermos-importer.spatial :as spatial]
            
            [thermos-backend.importer.process :as importer]
            [thermos-backend.solver.interop :as interop]
            [thermos-util :refer [as-double as-boolean]]
            [thermos-importer.lidar :as lidar]
            
            [thermos-specs.document :as document]
            [thermos-specs.defaults :as defaults]

            [thermos-specs.path :as path]
            [thermos-specs.measure :as measure]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.supply :as supply]
            [thermos-specs.tariff :as tariff]
            [mount.core :as mount]
            )
  (:gen-class))

;; THERMOS CLI tools for Net Zero Analysis

(def options
  [[nil "--base FILE"
    "An edn file containing the base scenario."]
   [nil "--map FILE"
    "A geo file containing some map stuff"
    :update-fn conj]
   [nil "--lidar FILE" "LIDAR inputs"
    :update-fn conj]
   [nil "--degree-days NUMBER" "Degree days"
    :parse-fn #(Double/parseDouble %)]
   [nil "--connect-to-connectors" "Whether to allow connecting to connectors"]
   [nil "--shortest-face LENGTH" "When finding face centers, shortest face length"
    :default 3
    :parse-fn #(Double/parseDouble %)]
   [nil "--solver PATH" "Path to the solver program"]
   [nil "--height-field FIELD" "A height field"]
   [nil "--resi-field FIELD" "A resi field"]
   [nil "--demand-field FIELD" "A kwh/yr field"]
   [nil "--peak-field FIELD" "A kwp field"]
   [nil "--insulation FILE"
    "Rules for associating insulation"
    :update-fn conj]
   [nil "--alternatives FILE"
    "Rules for associating alternatives/counterfactuals"
    :update-fn conj]
   [nil "--supply FILE"
    "Rules for associating supplies"
    :update-fn conj]
   [nil "--civils FILE"
    "Rules for associating civil costs"
    :update-fn conj]
   [nil "--tariffs FILE"
    "Rules for associating tariffs"
    :update-fn conj]
   ["-h" "--help" "ARGH!"]
   ]
  )

(defn- load-edns [files]
  (when files
    (apply concat
          (for [file files]
            (with-open [r (io/reader file)]
              (edn/read r))))))

(defn- match
  "Match ITEM, a map, against OPTIONS, a list of things with a :rule in them.
  A :rule is a tuple going [field pattern], so when we (get field item) it matches pattern (a regex literal)"
  [item options & {:keys [match] :or {match :thermos-cli/rule}}]
  
  (and item
       (filter
        (fn matches? [option]
          (let [rule (get match option)]
            (cond
              (= true rule)
              option

              (vector? rule)
              (let [[field pattern] rule
                    field-value (get item field)]
                (and field field-value pattern
                     (re-find pattern (str field-value)))
                ))))
        options)))

(defn- node-connect
  "Given a set of shapes, do the noding and connecting dance"
  [{crs ::geoio/crs features ::geoio/features}

   connect-to-connectors
   shortest-face-length]

  (let [is-line (comp boolean #{:line-string :multi-line-string})

        {lines true not-lines false}
        (group-by is-line features)

        lines (spatial/node-paths lines)

        [buildings roads]
        (spatial/add-connections
         crs not-lines lines
         :shortest-face-length shortest-face-length
         :connect-to-connectors false)]

    [buildings roads]))

(defn- generate-demands [buildings 
                         degree-days lidar

                         resi-field
                         height-field
                         peak-field
                         demand-field
                         ]
  (let [sqrt-degree-days (Math/sqrt degree-days)
        lidar-index (->> lidar
                         (map io/file)
                         (mapcat file-seq)
                         (filter #(and (.isFile %)
                                       (let [name (.getName %)]
                                         (or (.endsWith name ".tif")
                                             (.endsWith name ".tiff")))))
                         (lidar/rasters->index))

        ;; do lidar smash
        buildings (lidar/add-lidar-to-shapes buildings lidar-index)
        ]
    ;; run the model for each building
    (for [b buildings]
      (let [is-resi (as-boolean (or
                                 (not resi-field)
                                 (and resi-field
                                      (get resi-field b))))
            height (and height-field
                        (as-double (get height-field b)))
            peak (and peak-field
                      (as-double (get peak-field b)))
            demand (and demand-field
                        (as-double (get demand-field b)))]
        (-> b
            (assoc :residential is-resi)
            (cond->
                height
              (assoc :height height)

              demand
              (assoc :annual-demand demand))
            (importer/produce-demand sqrt-degree-days)
            (as-> x
                (assoc x :peak-demand
                       (or peak
                           (importer/run-peak-model
                            (:annual-demand x))))))))))

(defn- add-civils [paths civils]
  (for [path paths]
    (let [civil (first (match path civils))]
      (cond-> path
        civil
        (assoc path ::path/civil-cost-id (::path/civil-cost-id civil))))))

(defn- add-insulation [buildings insulation]
  (for [building buildings]
    (let [insulation (match building insulation)
          insulation (set (map ::measure/id insulation))]
      (assoc building ::demand/insulation insulation))))

(defn- add-alternatives [buildings alternatives]
  ;; need to find counterfactual match
  (for [building buildings]
    (let [alts (match building alternatives)
          counter (::supply/id (first (match building alternatives :match :thermos-cli/counterfactual-rule)))
          alts (set (map ::supply/id alts))
          alts (cond-> alts counter (disj counter))
          ]
      (cond-> building
        (seq alts)
        (assoc ::demand/alternatives alts)

        counter
        (assoc ::demand/counterfactual counter)))))

(defn- add-tariffs [buildings tariffs]
  (for [building buildings]
    (let [tariff (::tariff/id (first (match building tariffs)))]
      (cond-> building
        tariff (assoc ::tariff/id tariff)))))

(defn- select-supply-location [instance supply]
  ;; for now we find the largest demand

  (let [biggest-demand
        (first (sort-by ::demand/kwp #(compare %2 %1)
                        (vals (::document/candidates instance))))]

    (update-in instance [::document/candidates (::candidate/id biggest-demand)]
               (fn [candidate]
                 (merge candidate supply)))))

(defn --main [options]
  (mount/start-with {#'thermos-backend.config/config
                     {:solver-directory "."
                      :solver-command (:solver options)}})
  (let [
        geodata           (geoio/read-from-multiple (:map options))
        [paths buildings] (node-connect geodata
                                        (:connect-to-connectors options)
                                        (:shortest-face options))
        
        paths             (add-civils paths (:civils options))

        buildings         (-> buildings
                              (generate-demands    buildings
                                                   (:degree-days options)

                                                   (:lidar options)
                                                   (:resi-field options)
                                                   (:height-field options)
                                                   (:peak-field options)
                                                   (:demand-field options))
                              
                              (add-insulation      (:insulation options))
                              (add-alternatives    (:alternatives options))
                              (add-tariffs         (:tariffs options)))

        instance          (if-let [base (:base options)]
                            (load-edns [base])
                            defaults/default-document)

        instance          (assoc instance
                                 ::document/tariffs      (group-by ::tariff/id (:tariffs options))
                                 ::document/civil-costs  (group-by ::path/civil-cost-id (:civils options))
                                 ::document/insulation   (group-by ::measure/id (:insulation options))
                                 ::document/alternatives (group-by ::supply/id (:alternatives options)))

        ;; TODO find a supply location to consider

        instance          (select-supply-location instance (:supply options))
        solution          (interop/solve "job" instance)
        ])

  (mount/stop))

(defn- generate-ids [things id]
  (map-indexed things (fn [i t] (assoc t id i))))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args options)

        options (-> options
                    (update :insulation   load-edns)
                    (update :alternatives load-edns)
                    (update :tariffs      load-edns)
                    (update :civils       load-edns)
                    (update :supply       load-edns)

                    (update :insulation   generate-ids ::measure/id)
                    (update :alternatives generate-ids ::supply/id)
                    (update :tariffs      generate-ids ::tariff/id)
                    (update :civils       generate-ids ::path/civil-cost-id)
                    )]
    (cond
      (:help options)
      (println summary)

      :else
      (--main options)
      )
    ))

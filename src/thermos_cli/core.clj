(ns thermos-cli.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [thermos-importer.geoio :as geoio]
            [thermos-specs.document :as document]
            [thermos-specs.defaults :as defaults]

            [thermos-specs.path :as path]
            [thermos-specs.measure :as measure]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.demand :as demand]
            [thermos-specs.supply :as supply]
            [thermos-specs.tariff :as tariff])
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
   [nil "--height-field FIELD" "A height field"]
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
  (apply merge
         (for [file files]
           (with-open [r (io/reader file)]
             (edn/read r)))))

(defn- match
  "Match ITEM, a map, against OPTIONS, a list of things with a :rule in them.
  A :rule is a tuple going [field pattern], so when we (get field item) it matches pattern (a regex literal)"
  [item options & {:keys [match] :or {match :rule}}]
  
  (and item
       (filter
        (fn matches? [option]
          (let [[field pattern] (get match option)
                field-value (get item field)]
            (and field field-value pattern
                 (re-find pattern (str field-value)))))
        options)))

(defn- node-connect [geodata]
  
  )

(defn- generate-demands [buildings lidar height-fields peak-fields degree-days]
  
  )

(defn- add-civils [paths civils]
  (let [civils (vals civils)]
    (for [path paths]
      (let [civil (first (match path civils))]
        (cond-> path
          civil
          (assoc path ::path/civil-cost-id (::path/civil-cost-id civil)))))))

(defn- add-insulation [buildings insulation]
  (let [insulation (vals insulation)]
    (for [building buildings]
      (let [insulation (match building insulation)
            insulation (set (map ::measure/id insulation))]
        (assoc building ::demand/insulation insulation)))))

(defn- add-alternatives [buildings alternatives]
  ;; need to find counterfactual match
  (let [alternatives (vals alternatives)]
    (for [building buildings]
      (let [alts (match building alternatives)
            counter (::supply/id (first (match building alternatives :match :counterfactual-rule)))
            alts (set (map ::supply/id alts))
            alts (cond-> alts counter (disj counter))
            ]
        (cond-> building
          (seq alts)
          (assoc ::demand/alternatives alts)

          counter
          (assoc ::demand/counterfactual counter))))))

(defn- add-tariffs [buildings tariffs]
  (let [tariffs (vals tariffs)]
    (for [building buildings]
      (let [tariff (::tariff/id (first (match building tariffs)))]
        (cond-> building
          tariff (assoc ::tariff/id tariff))))))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args options)

        options (-> options
                    (update :insulation   load-edns)
                    (update :alternatives load-edns)
                    (update :tariffs      load-edns))

        geodata           (geoio/read-from-multiple (:map options))
        [paths buildings] (node-connect geodata)

        paths             (add-civils paths (:civils options))

        buildings         (-> buildings
                              (generate-demands    (:lidar options)
                                                   (:height-field options)
                                                   (:peak-field options)
                                                   (:degree-days options))
                              (add-insulation      (:insulation options))
                              (add-alternatives    (:alternatives options))
                              (add-tariffs         (:tariffs options)))

        instance          (if-let [base (:base options)]
                            (load-edns [base])
                            defaults/default-document)

        instance          (assoc instance
                                 ::document/tariffs      (:tariffs options)
                                 ::document/civil-costs  (:civils options)
                                 ::document/insulation   (:insulation options)
                                 ::document/alternatives (:alternatives options))

        ;; find supply location! 
        ]
    

    
    

    )
  )

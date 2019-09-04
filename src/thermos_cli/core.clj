(ns thermos-cli.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [thermos-importer.geoio :as geoio]
            [thermos-specs.document :as document]
            [thermos-specs.defaults :as defaults]
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

(defn load-edns [files]
  (apply merge
         (for [file files]
           (with-open [r (io/reader file)]
             (edn/read r)))))

(defn- node-connect [geodata]
  
  )

(defn- add-civils [paths civils]
  
  )

(defn- generate-demands [buildings lidar height-fields peak-fields degree-days]
  
  )

(defn- add-insulation [buildings insulation-rules])
(defn- add-alternatives [buildings alternative-rules])
(defn- add-tariffs [buildings tariff-rules])

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

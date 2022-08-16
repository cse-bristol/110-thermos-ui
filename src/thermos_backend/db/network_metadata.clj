(ns thermos-backend.db.network-metadata
  "Function for summarising a thermos problem so we can quickly present
  some facts about it into the UI"
  (:require [thermos-specs.candidate :as candidate]
            [thermos-specs.document :as document]
            [thermos-specs.supply :as supply]
            [thermos-specs.path :as path]
            [thermos-specs.solution :as solution]))

(declare input-summary output-summary)

(defn summarise [problem]
  (cond-> (input-summary problem)
    (document/has-solution? problem)
    (merge (output-summary problem))))

(defn input-summary [problem]
  (let [{paths :path
         buildings :building}
        (document/candidates-by-type problem)
        mode (document/mode problem)

        supplies (filter candidate/has-supply? buildings)
        ]
    {:mode (name mode)
     :objective (name (::document/objective problem :network))
     :npv-term  (::document/npv-term problem)
     :npv-rate  (::document/npv-rate problem)
     :supply-capacities    (map ::supply/capacity-kwp supplies)
     :supply-prices        (map ::supply/cost-per-kwh supplies)
     :input-building-count (count buildings)
     :input-path-length    (reduce + 0 (keep ::path/length paths))
     :input-building-kwh   (reduce + 0 (keep #(candidate/annual-demand % mode) buildings))
    }))

(defn output-summary [problem]
  (let [mode (document/mode problem)
        solution-state (::solution/state problem)
        {paths :path
         buildings :building}
        (document/candidates-by-type problem)

        supplies (filter candidate/supply-in-solution? buildings)

        buildings-by-system
        (dissoc (group-by candidate/system-name buildings) nil)
        ]
    {:solution-state   (name solution-state)
     :runtime          (::solution/runtime problem)
     :mip-gap          (::solution/gap problem)
     :objective-value  (::solution/objective problem)
     :network-length   (->> paths
                            (filter candidate/in-solution?)
                            (keep ::path/length)
                            (reduce + 0))
     :network-count    (->> buildings
                            (filter candidate/is-connected?)
                            (count))
     :network-kwh      (->> supplies
                            (keep ::solution/output-kwh)
                            (reduce + 0))
     :network-kwp      (->> supplies
                            (keep ::solution/capacity-kw)
                            (reduce + 0))
     :output-building-kwh (into
                           {}
                           (for [[system-name buildings] buildings-by-system]
                             [system-name
                              (reduce + 0
                                      (keep #(candidate/solved-annual-demand % mode)
                                            buildings-by-system))]))
     :output-building-kwp (into
                           {}
                           (for [[system-name buildings] buildings-by-system]
                             [system-name
                              (reduce + 0
                                      (keep #(candidate/solved-peak-demand % mode)
                                            buildings-by-system))]))}))


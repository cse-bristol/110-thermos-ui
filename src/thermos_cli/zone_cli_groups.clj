(ns thermos-cli.zone-cli-groups
  (:require [thermos-cli.zone-cli-io :as zone-io]
            [thermos-specs.demand :as demand]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.solution :as solution]
            [thermos-specs.supply :as supply]
            [thermos-specs.path :as path]
            [thermos-specs.document :as document]
            [clojure.string :as string]
            [thermos-backend.solver.interop :as interop]))

(defn building-group
  "Find the first non-nil or non-blank field on building from group-fields"
  [group-fields building]
  (some (fn [field]
          (let [val (get building field)]
            (if (string? val)
              (if (string/blank? val) nil
                  (string/trim val))
              val)))
        group-fields))

(defn- round-groups
  "Apply rounding to the groups of buildings in `problem`

  Return a tuple [rounding decisions, fixed problem]
  "
  [problem parameters]
  (let [buildings (filter (comp #{:building} ::candidate/type)
                          (vals (::document/candidates problem)))

        group-fields               (:rounding/group-buildings-by parameters)
        building-group             (partial building-group group-fields)
        
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
                  (let [group     (building-group building)
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
           (let [group (building-group building)
                 decision (if (or (nil? group)
                                  ;; we should only round optional
                                  ;; buildings - if we forced the
                                  ;; building on or off by rules
                                  ;; we want to stick to that.
                                  (not= :optional (::candidate/inclusion building)))
                            :skip
                            (get decisions group :skip))
                 inclusion (case decision
                             :up   :required
                             :down :individual
                             :skip (if (candidate/is-connected? building)
                                     :required :individual))

                 building (assoc building
                                 ::candidate/inclusion inclusion
                                 ::rounding-group   (and group (str group))
                                 ::rounded-building (if (or (and (= inclusion :required)
                                                                 (candidate/is-connected? building))
                                                            (and (= inclusion :individual)
                                                                 (not (candidate/is-connected? building))))
                                                      "skip"
                                                      (name decision)))

                 building (if (= :individual inclusion)
                            (let [alternative (::solution/alternative building)]
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
             building )))]
    [(for [[group stats] groups]
       (assoc stats
              :decision (get decisions group)
              :group group))
     problem]))

(defn set-optimiser-group [building group-fields]
  (cond-> building
    (seq group-fields)
    (assoc ::demand/group (building-group group-fields building))))

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

(defn round-solution [{:keys [input-file output-file parameters
                               runtime output-geometry]}]
  ;; TODO we could say if there is no heat price cheat and output the input
  (let [parameters (zone-io/read-edn parameters)
        solution   (zone-io/read-edn input-file)

        [rounding-decisions rounded-problem]
        (round-groups solution parameters)

        rounded-problem
        (fix-supply-choice rounded-problem)

        rounded-problem
        (cond-> rounded-problem
          runtime
          (assoc ::document/maximum-runtime (double (/ runtime 3600))))

        ;; disable infill rules in case there are any
        rounded-problem
        (dissoc rounded-problem ::document/infill-targets)
        
        rounded-solution
        (interop/try-solve rounded-problem (fn [& _]))

        {buildings :building
         paths :path}  (document/candidates-by-type rounded-solution)
        
        supplies (filter candidate/supply-in-solution? buildings)]

    (zone-io/output-metadata rounded-solution true output-file)

    (zone-io/write-sqlite
     output-file
     "rounding"
     [["group" :string (comp str :group)]
      ["count" :int :n]
      ["value_in" :double  #(double (get % true 0.0))]
      ["value_out" :double #(double (get % false 0.0))]
      ["decision" :string   (comp name :decision)]]
     rounding-decisions)

    (zone-io/output rounded-solution
            buildings
            paths
            supplies
            output-file
            "EPSG:27700" ;; urgh no
            output-geometry)

    rounded-solution))

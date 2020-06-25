(ns thermos-backend.solver.supply
  "Interop with the supply-model & solver"
  (:require [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [thermos.opt.supply.profiles :as profiles]
            [thermos.opt.supply.core :as supply-model]
            [thermos-specs.solution :as solution]
            [thermos-specs.supply :as supply]
            [lp.scip :as scip]
            [clojure.tools.logging :as log]
            [thermos-util :as util]))

(defn- de-sparsify [values divisions]
  {:pre [(or (map? values)
             (nil? values)
             (list? values)
             (vector? values))]}
  (cond
    (map? values)
    (vec (for [i (range divisions)] (get values i 0)))

    (nil? values)
    (vec (repeat divisions 0))

    :else
    (vec values)))

(defn solve [label doc]
  ;; We are going to assume for now a single supply location
  ;; which logically must serve all demands

  ;; If there is no network solution, we need some plan B
  
  (let [solution-buildings
        (filter #(and (candidate/is-building? %)
                      (candidate/in-solution? %))
                (vals (::document/candidates doc)))

        demands  (filter candidate/is-connected? solution-buildings)
        supplies (filter candidate/supply-in-solution? solution-buildings)
        
        mode     (document/mode doc)
        
        supply (first supplies)

        day-types     (::supply/day-types doc)
        heat-profiles (::supply/heat-profiles doc)
        fuels         (::supply/fuels doc)
        grid-offer    (::supply/grid-offer doc)
        
        default-profile-id (or (::supply/default-profile doc)
                               (document/minimum-key heat-profiles))
        
        ;; we have load profiles across buildings, which we want to
        ;; merge

        plant-load (profiles/combine-buildings
                       day-types
                       ;; we only need id=>values for heat profiles
                       (into {} (for [[id {values :demand}] heat-profiles]
                                  [id
                                   (into {}
                                         (for [[day-type values] values]
                                           [day-type (de-sparsify values
                                                                  (:divisions (get day-types day-type)))]))]))
                       

                       ;; for each building we just need profile id, kwh, kwp
                       (for [c demands]
                         {:profile (::supply/profile-id c default-profile-id)
                          :kwh     (candidate/annual-demand c mode)
                          :kwp     (candidate/peak-demand c mode)})

                       ;; and these are the target values for the combined curve
                       (::solution/output-kwh supply 0)
                       (::solution/capacity-kw supply 0))

        substations     (::supply/substations doc)
        
        ;; this gives us the load-profile for the plant
        ;; which we now need to merge with fuel profiles
        ;; to get the final input for the supply model
        ;; that has the form

        ;; {day => {frequency, [heat demand], [grid offer], fuel {:price, :co2 etc}}}
        input-profile
        (into
         {}
         (for [[day-type {f :frequency
                          d :divisions}] day-types]
           [day-type
            {:frequency   f
             :divisions   d
             :heat-demand (de-sparsify (:values (get plant-load day-type)) d)
             :grid-offer  (de-sparsify (get grid-offer day-type) d)
             :substation-load-kw
             (into
              {}
              (for [[substation-id {:keys [load-kw]}] substations]
                [substation-id (de-sparsify (get load-kw day-type) d)]))
             ;; fuel type in the document goes
             ;; {fuel id => {:values => {day type => [vals]}}}
             ;; but we want {day type => {:fuel => {fuel id => [vals]}}}
             :fuel
             (into
              {}
              (for [[fuel-type {:keys [price co2 nox pm25]}] fuels]
                [fuel-type
                 {:price (de-sparsify (get price day-type) d)
                  :co2   (de-sparsify (get co2 day-type) d)
                  :nox   (de-sparsify (get nox day-type) d)
                  :pm25  (de-sparsify (get pm25 day-type) d)}]))
             }]))
        
        
        plant-options   (::supply/plants doc)
        storage-options (::supply/storages doc)


        objective-params (::supply/objective doc)
        
        ;; construct supply problem from our document
        supply-problem
        {:curtailment-cost  (:curtailment-cost objective-params 1000.0)
         :can-dump-heat     (:can-dump-heat objective-params false)
         :discount-rate     (:discount-rate objective-params 0.0)
         :accounting-period (:accounting-period objective-params 1)

         :co2-price  (-> doc ::document/emissions-cost (:co2 0.0))
         :nox-price  (-> doc ::document/emissions-cost (:nox 0.0))
         :pm25-price (-> doc ::document/emissions-cost (:pm25 0.0))
         
         :profile input-profile

         :plant-options   plant-options
         :storage-options (or storage-options {})
         :substations     (or substations {})

         :day-types day-types ;; pass them on, to preserve names
         :fuels     (into
                     {}
                     (for [[fuel-id fuel-attrs] fuels]
                       [fuel-id (select-keys fuel-attrs [:name])]))
         }

        solution (-> (supply-model/construct-mip supply-problem)
                     (scip/solve :time-limit (:time-limit objective-params 1.0)
                                 :mip-gap    (:mip-gap objective-params 0.05))
                     (supply-model/interpret-solution supply-problem))
        ]
    ;; add info to doc
    (assoc doc
           ::solution/supply-solution solution
           ::solution/supply-problem supply-problem)))

(defn try-solve [label doc]
  (try
    (solve label doc)
    (catch Throwable ex
      (log/error "Uncaught exception solving supply problem" ex)
      (-> doc
          (dissoc ::solution/supply-solution)
          (assoc ::solution/supply-solution
                 {:state :uncaught-error
                  :log (with-out-str
                         (clojure.stacktrace/print-throwable ex)
                         (println)
                         (println "---")
                         (clojure.stacktrace/print-cause-trace ex))
                  } )))))

;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-util.solution-summary
  (:require [thermos-specs.solution :as solution]
            [thermos-specs.document :as document]
            [thermos-specs.supply :as supply]
            [thermos-specs.measure :as measure]
            [thermos-specs.candidate :as candidate]
            [thermos-util.finance :as finance]
            [com.rpl.specter :as S]))

(defn data-table [document capex-mode opex-mode]
  (let [solution-members (->> document
                              ::document/candidates
                              vals
                              (filter candidate/in-solution?))
        {paths :path
         buildings :building}
        (group-by ::candidate/type solution-members)

        alts    (filter ::solution/alternative buildings)
        by-alt  (sort-by first
                         (group-by (comp ::supply/name ::solution/alternative) alts))
        insulated
        (filter (comp seq ::solution/insulation) buildings)

        by-insulation
        (->> (mapcat ::solution/insulation insulated)
             (group-by ::measure/name)
             (sort-by first))
        
        supplies (filter candidate/supply-in-solution? buildings)

        demands (filter candidate/is-connected? buildings)
        
        {model-mode ::document/mode
         npv-term   ::document/npv-term
         npv-rate   ::document/npv-rate} document

        sum-cost-outputs
        (fn [& {:keys [capex opex revenue]}]
          (let [tcapex (when capex   (reduce + (map capex-mode capex)))
                topex  (when opex    (reduce + (map opex-mode opex)))
                trev   (when revenue (reduce + (map opex-mode revenue)))
                tpc    (+ (reduce + 0 (map :present capex))
                          (reduce + 0 (map :present opex)))
                tpv    (reduce + 0 (map :present revenue))
                tnpv   (- tpv tpc)]
            {:capex tcapex :opex topex :revenue trev :present tnpv
             :present-cost tpc}))

        total-demand
        (fn [bs]
          (reduce
           (fn [a c] (+ a (candidate/solved-annual-demand c model-mode))) 0
           bs))

        sum-summed-costs ;; for adding up :value terms
        (fn [a b]
          (merge-with
           (fn [a b] (and (or a b) (+ (or a 0) (or b 0))))
           a b))

        equivalized-cost
        (fn [kwh pv]
          (if (and (number? kwh) (number? pv))
            (* 100
               (/ pv (finance/pv npv-rate (repeat npv-term kwh))))
            nil))

        rows
        [{:name "Network"
          :subcategories
          [{:name "Pipework"
            :value
            (sum-cost-outputs
             :capex (map ::solution/pipe-capex paths))}

           {:name "Heat supply"
            :value
            (sum-cost-outputs
             :capex (map ::solution/supply-capex supplies)
             :opex  (mapcat (juxt ::solution/supply-opex
                                  ::solution/heat-cost
                                  ::solution/pumping-cost)
                            supplies))}

           {:name "Demands"
            :value
            (sum-cost-outputs
             :capex   (map ::solution/connection-capex demands)
             :revenue (map ::solution/heat-revenue demands))}

           {:name "Emissions"
            :value
            (sum-cost-outputs
             :opex (flatten (mapcat (juxt (comp vals ::solution/supply-emissions)
                                          (comp vals ::solution/pumping-emissions)) supplies)))}]

          :kwh (total-demand demands)}

         {:name "Individual Systems"
          :subcategories
          (-> (for [[alt-name alts] by-alt]
                {:name alt-name
                 :value
                 (sum-cost-outputs
                  :capex (map (comp :capex ::solution/alternative) alts)
                  :opex (mapcat
                         (fn [b]
                           (let [a (::solution/alternative b)]
                             (-> (vals (:emissions a))
                                 (conj (:heat-cost a)
                                       (:opex a)))))
                         alts))
                 :kwh  (total-demand alts)})
              (vec)
              (conj
               {:name "Emissions"
                :value
                (sum-cost-outputs
                 :opex (mapcat (comp vals :emissions ::solution/alternative) alts))}))}

         {:name "Insulation"
          :subcategories
          (vec (for [[ins-name ins] by-insulation]
                 {:name ins-name :value (sum-cost-outputs :capex ins)}))}]

        rows
        (->> rows
             (S/transform [S/ALL :subcategories S/ALL]
                          (fn [subcategory]
                            (let [kwh (:kwh subcategory)
                                  pv (get-in subcategory [:value :present-cost])]
                              (assoc subcategory :equivalized-cost (equivalized-cost kwh pv)))))
             (map (fn [row]
                    (let [to-sum (map :value (:subcategories row))
                          summed (when-not (empty? to-sum) (reduce sum-summed-costs to-sum))
                          kwh (:kwh row)
                          pv (:present-cost summed)
                          summed (assoc summed :equivalized-cost (equivalized-cost kwh pv))]
                      (assoc row :total summed)))))

        grand-total
        (let [gt (->> rows
                      (mapcat :subcategories)
                      (map :value)
                      (reduce sum-summed-costs))]
          (assoc gt :present
                 (- (:present gt)
                    (reduce + 0
                            (keep :present (map (comp ::solution/heat-revenue) demands))))))]
    {:rows rows
     :grand-total grand-total}))
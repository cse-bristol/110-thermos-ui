;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-util.finance
  (:require [thermos-specs.document :as document]
            [hnzp-utils.finance :as finance]
            [thermos-util
             :refer [safe-div]
             :refer-macros [safe-div]]))

(def ^:dynamic *npv-rate* 0.0)
(def ^:dynamic *npv-term* 1)

(defn pv [vals] (finance/pv *npv-rate* *npv-term* vals))

(def sum-series finance/sum-series)
(def up-front finance/up-front)
(def recurring finance/recurring)
(def annual finance/annual)

(defn up-front+recurring [lifetime cost]
  (sum-series (up-front cost) (recurring lifetime cost)))

(comment
  (defn objective-capex-value [doc opts value exists]
    (let [{should-annualize :annualize
           should-recur :recur
           period :period
           loan-rate :rate}
          opts

          period    (max 1 (or period 1))
          loan-rate (or loan-rate 0)

          {npv-rate ::document/npv-rate
           npv-term ::document/npv-term} doc

          npv-rate (or npv-rate 0)
          npv-term (max 1 (or npv-term 1))
          
          payments
          (if should-annualize
            (annualize loan-rate period value)
            (take (max 1 period)
                  (concat [value] (repeat 0))))

          payments
          (if should-recur
            (take npv-term (cycle payments))
            payments)

          payments
          (if exists
            (concat (repeat period 0)
                    (drop period payments))
            payments)
          
          total-value (reduce + payments)
          ]
      {:present (pv npv-rate payments)
       :total total-value
       :annual (safe-div total-value period)
       :principal value}))

  (defn objective-opex-value [doc value]
    (let [payments (repeat (::document/npv-term doc 1) value)
          total (reduce + payments)]
      {:present (pv (::document/npv-rate doc 0) payments)
       :total total
       :annual value}))

  (defn adjusted-value
    "Determine the objective function contribution for a type of cost.
  This is affected by settings in the document."
    [doc type value]
    (-> (case type
          :connection-capex
          (objective-capex-value doc
                                 (get-in doc [::document/capital-costs :connection])
                                 value
                                 false)
          
          :supply-capex
          (objective-capex-value doc
                                 (get-in doc [::document/capital-costs :supply])
                                 value
                                 false)
          
          :pipe-capex
          (objective-capex-value doc
                                 (get-in doc [::document/capital-costs :pipework])
                                 value
                                 false)

          :existing-pipe-capex
          (objective-capex-value doc
                                 (get-in doc [::document/capital-costs :pipework])
                                 value
                                 true)
          
          :insulation-capex
          (objective-capex-value doc
                                 (get-in doc [::document/capital-costs :insulation])
                                 value
                                 false)
          
          :alternative-capex
          (objective-capex-value doc
                                 (get-in doc [::document/capital-costs :alternative])
                                 value
                                 false)

          :counterfactual-capex
          (objective-capex-value doc
                                 (get-in doc [::document/capital-costs :alternative])
                                 value
                                 true)

          (:supply-heat :supply-opex :emissions-cost :heat-revenue :alternative-opex :supply-pumping)

          ;; these things are not capexes so we account for them the other way
          (objective-opex-value doc value))
        (assoc :type type)
        ))

  (defn objective-value [doc type value]
    (:present (adjusted-value doc type value)))

  (defn emissions-value [doc em kg]
    (let [kg (or kg 0)
          price (get (::document/emissions-cost doc) em 0)]
      (assoc (adjusted-value doc :emissions-cost (* price kg))
             :type em
             :kg kg))))

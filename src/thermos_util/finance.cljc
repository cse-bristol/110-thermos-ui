(ns thermos-util.finance
  (:require [thermos-specs.document :as document]))

(defn pv
  "Calculate the NPV of a series of values given a discount rate"
  [npv-rate vals]

  (if (zero? npv-rate)
    (reduce + vals)
    (reduce + (map-indexed (fn [i v] (/ v (Math/pow (+ 1 npv-rate) i))) vals))))

(defn annualize
  "Convert a capital cost right now into a series of repayments over time"
  [loan-rate loan-term principal]

  (if (zero? loan-rate)
    (repeat loan-term (/ principal loan-term))
    
    (let [repayment (/ (* principal loan-rate)
                       (- 1 (/ 1 (Math/pow (+ 1 loan-rate)
                                           loan-term))))]
      (repeat loan-term repayment))))

(defn objective-capex-value [doc opts value]
  (let [{should-annualize :annualize
         should-recur :recur
         period :period
         loan-rate :rate}
        opts

        period (or period 1)
        loan-rate (or loan-rate 0)

        {npv-rate ::document/npv-rate
         npv-term ::document/npv-term} doc

        npv-rate (or npv-rate 0)
        npv-term (or npv-term 1)
        
        payments
        (if should-annualize
          (annualize loan-rate period value)
          (take (max 1 period)
                (concat [value] (repeat 0))))

        payments
        (if should-recur
          (take npv-term (cycle payments))
          payments)]
    (pv npv-rate payments)))

(defn objective-value
  "Determine the objective function contribution for a type of cost.
  This is affected by settings in the document."
  [doc type value]
  (case type
    :connection-capex
    (objective-capex-value doc
                           (get-in doc [::document/capital-costs :connection])
                           value)
    
    :supply-capex
    (objective-capex-value doc
                           (get-in doc [::document/capital-costs :supply])
                           value)
    
    :pipe-capex
    (objective-capex-value doc
                           (get-in doc [::document/capital-costs :pipework])
                           value)
    
    :insulation-capex
    (objective-capex-value doc
                           (get-in doc [::document/capital-costs :insulation])
                           value)
    
    :alternative-capex
    (objective-capex-value doc
                           (get-in doc [::document/capital-costs :alternative])
                           value)

    
    (:supply-opex :emissions-cost :heat-revenue :alternative-opex)

    ;; these things are not capexes so we account for them the other way
    (pv (::document/npv-rate doc 0)
        (repeat (::document/npv-term doc 1) value))))


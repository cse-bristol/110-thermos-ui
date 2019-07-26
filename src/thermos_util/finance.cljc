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

(defn objective-value [doc type value]
  (case type
    (:connection :supply-capex :pipe-capex)
    ;; these things we take a loan for
    (pv (::document/npv-rate doc 0)
        (annualize (::document/loan-rate doc 0)
                   (::document/loan-term doc 1)
                   value))
    
    (:supply-opex :emissions-cost :heat-revenue)
    ;; these things we do not
    (pv (::document/npv-rate doc 0)
        (repeat (::document/npv-term doc 1) value))))


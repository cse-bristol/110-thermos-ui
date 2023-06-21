;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.solver.market
  (:require [thermos-util :as util]
            [thermos-util.finance :refer [pv]]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.supply :as supply]
            [thermos-specs.measure :as measure]
            [thermos-specs.tariff :as tariff]))

(defn- evaluate-base-case [term discount-rate kwp kwh-per-year n-customers emissions-costs alternative]
  (let [;; basic costs
        capex (supply/principal alternative kwp kwh-per-year n-customers)
        opex  (supply/opex alternative kwp n-customers)
        fuel  (supply/heat-cost alternative kwh-per-year)

        ;; we need unit rates so we can do decisions later
        capex-per-kwh (* (::supply/capex-per-mean-kw alternative 0) (util/annual-kwh->kw 1))
        fuel-per-kwh  (::supply/cost-per-kwh alternative 0)


        ;; emissions costs
        emissions-factors (::supply/emissions alternative)
        
        emissions-cost-per-kwh
        (reduce + 0
                (for [e candidate/emissions-types]
                  (* (get emissions-costs e 0)
                     (get emissions-factors e 0))))

        emissions-cost (* kwh-per-year emissions-cost-per-kwh)


        ;; We need to make these make sense.
        ;; Elsewhere, capital costs have options for finance and repeating
        ;; here we do not care for finance, but we should perhaps care for repeating.
        present-cost    (+ capex
                           (pv discount-rate (repeat term (+ opex fuel emissions-cost))))
        
        abatement-value (+ capex-per-kwh
                           (pv discount-rate (repeat term
                                       (+ fuel-per-kwh
                                          emissions-cost-per-kwh))))]
    

    {::supply/id       (::supply/id alternative)
     ::present-cost    present-cost
     ::abatement-value abatement-value
     }))

(defn evaluate
  "Work out what the best individual option for a given demand is.

  This is

  foreach individual option
     work out pv
     foreach insulation
       work out pv, if better add on insulation

  The result is choice of alternative system (or counterfactual) & insulation options.

  We also know the pv for this so we can work out an equivalent unit rate."
  [term discount-rate ;; for householder's PV calculation
   stickiness         ;; % network has to beat alternative by to win
   kwp kwh-per-year n-customers   ;; for working out the costs / benefits
   areas              ;; area type => area
   emissions-costs
   alternatives       ;; list of alternatives
   insulations        ;; list of insulations; these need other parameters to work
   ]

  (if (empty? alternatives)
    0.0
    (let [insulated-alternatives
          ;; foreach alternative work out which insulation it should get
          
          (for [a alternatives]
            (let [a0 (evaluate-base-case term discount-rate
                                         kwp kwh-per-year n-customers
                                         emissions-costs
                                         a)
                  
                  abatement-value (::abatement-value a0)
                  ]
              (reduce
               (fn [r i]
                 (let [{fixed-cost  ::measure/fixed-cost
                        cost-per-m2 ::measure/cost-per-m2
                        max-effect  ::measure/maximum-effect
                        max-area    ::measure-maximum-area
                        surface     ::measure/surface
                        id          ::measure/id
                        } i

                       area (get areas surface 0)
                       max-cost (+ (or fixed-cost 0) (* (or cost-per-m2 0)
                                                        (or max-area 0)))
                       max-kwh  (* (or max-effect 0) kwh-per-year)

                       ;; this is the present value of buying the insulation
                       max-abatement-value (* abatement-value max-kwh)
                       saving (- max-abatement-value max-cost)
                       ]

                   (cond-> r
                     (pos? saving) ;; it is worth it
                     (-> (update ::measure/id conj id)
                         (update ::present-cost - saving)))))
               a0
               insulations)))

          best-alternative
          (apply min-key ::present-cost insulated-alternatives)

          best-cost (::present-cost best-alternative)
          our-offer (* best-cost (- 1.0 stickiness))

          ;; this is a present cost, so our question is what unit rate gets us this cost.
          ;; I'm not sure if there's an easy analytical solution or not

          ;; we want offer = sum_i (k . v / (r ^ i)), solve for k; seems the PV is separable
          pv-per-kwh (pv discount-rate (repeat term kwh-per-year))
          ;; offer = k . v . pv1
          ;; k = offer / v.pv1

          offer-rate (/ our-offer pv-per-kwh)

          ;; round to nearest 0.1p?
          offer-rate (/ (Math/round (* 1000.0 offer-rate)) 1000.0)
          ]
      (assoc best-alternative
             ::unit-rate offer-rate
             ::tariff/unit-charge offer-rate
             ))))

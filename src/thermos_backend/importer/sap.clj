;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.importer.sap)

(let [sap-delta-t
      (double-array [41.2 41.4 37.6 36.4 33.9 30.4 33.4 33.5 36.3 39.4 39.9])

      sap-volume-factor
      (double-array [1.1 1.06 1.02 0.98 0.94 0.90 0.90 0.94 0.98 1.02 1.06 1.1])

      sap-days
      ;;             J  F  M  A  M  J  Jy A  S  O  N  D
      (double-array [31 28 31 30 31 30 31 31 30 31 30 31])

      ;; This is from Sap Table 3: Primary circuit loss
      
      primary-losses
      (areduce sap-days month total 0
               (+ total (* (aget sap-days month)
                           14
                           ;;        ↓ this is [0.0091 × p + 0.0245 × (1-p)] × h
                           ;;        ↓ but where p = 1 and h = 3, which is
                           ;;        ↓ fully insulated primary & cylinder stat & prog.
                           (+ 0.0263 (* 0.0091 3))
                           )))
      ]
  
  (defn hot-water ^double [^double floor-area]
    (let [sap-occupancy
          (if (> floor-area 13.9)
            (+ 1
               (* 1.76 (- 1 (Math/exp
                             (* -0.000349
                                (Math/pow (- floor-area 13.9) 2.0)))))
               
               (* 0.0013 (- floor-area 13.9)))
            1)
          
          sap-liters-per-day
          (+ (* 25 sap-occupancy) 36)]
      
      (+
       primary-losses
       (areduce
        sap-delta-t
        month total 0

        (+ total
           (* 4.18
              sap-liters-per-day
              (aget sap-days month)
              (aget sap-volume-factor month)
              (aget sap-delta-t month)
              (/ 3600.0)))))
      )))

        

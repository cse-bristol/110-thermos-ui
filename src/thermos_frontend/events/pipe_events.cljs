;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.events.pipe-events
  (:require [thermos-frontend.events.core :refer [handle]]
            [thermos-specs.document :as document]
            [thermos-specs.path :as path]
            [com.rpl.specter :as sr]
            [thermos-util :refer [assoc-id]]
            ))

(defmethod handle :pipe/change-diameter
  [state [_ from to]]
  (sr/setval
   [::document/pipe-costs
    :rows
    sr/MAP-KEYS
    (sr/pred= from)]
   to
   state))

(comment
  (-> {::document/pipe-costs
       {:rows {100 {:a 1} 200 {:b 1}}}}
      (handle [:pipe/change-diameter 100 150])
      (= {::document/pipe-costs
          {:rows {150 {:a 1} 200 {:b 1}}}}))
  )

(defmethod handle :pipe/rename-civils
  [state [_ cid name]]
  (assoc-in state [::document/pipe-costs :civils cid] name))

(defmethod handle :pipe/change-cost
  [state [_ dia cost]]
  (assoc-in state [::document/pipe-costs :rows dia :pipe] cost))

(defmethod handle :pipe/change-civil-cost
  [state [_ dia cid cost]]
  (assoc-in state [::document/pipe-costs :rows dia cid] cost))

(defmethod handle :pipe/add-diameter
  [state [_ dia]]
  (assoc-in
   state
   [::document/pipe-costs :rows dia]
   {}
   )
  )

(defmethod handle :pipe/add-civils
  [state [_ n]]
  (update-in
   state
   [::document/pipe-costs :civils]
   assoc-id n))

(defmethod handle :pipe/remove-diameter
  [state [_ dia]]
  (update-in state [::document/pipe-costs :rows]
             dissoc dia))

(defmethod handle :pipe/remove-civils
  [state [_ cid]]
  (document/remove-civils state cid))

(defmethod handle :pipe/change-capacity
  [state [_ dia capacity-kw]]
  (if (number? capacity-kw)
    (assoc-in state
              [::document/pipe-costs :rows dia :capacity-kw]
              capacity-kw)
    (update-in state
               [::document/pipe-costs :rows dia]
               dissoc :capacity-kw)))

(defmethod handle :pipe/change-losses
  [state [_ dia losses-kwh]]
  (if (number? losses-kwh)
    (assoc-in state
              [::document/pipe-costs :rows dia :losses-kwh]
              losses-kwh)
    (update-in state
               [::document/pipe-costs :rows dia]
               dissoc :losses-kwh)))

(defmethod handle :pipe/change-flow-temperature
  [state [_ t]]
  (assoc state ::document/flow-temperature t))

(defmethod handle :pipe/change-return-temperature
  [state [_ t]]
  (assoc state ::document/return-temperature t))

(defmethod handle :pipe/change-ground-temperature
  [state [_ t]]
  (assoc state ::document/ground-temperature t))

(defmethod handle :pipe/change-medium
  [state [_ m]]
  (assoc state ::document/medium m))

(defmethod handle :pipe/change-steam-pressure
  [state [_ p]]
  (assoc state ::document/steam-pressure p))

(defmethod handle :pipe/change-steam-velocity
  [state [_ v]]
  (assoc state ::document/steam-velocity v))

(defmethod handle :pipe/set-default-civils
  [state [_ d]]
  (assoc-in state [::document/pipe-costs :default-civils] d))

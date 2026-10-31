;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-util.pipes
  "Spot calculations of pipe capacity and losses for the UI.

  The model itself uses thermos.opt.net.pipes (see
  `thermos-backend.solver.interop/doc->pipe-curves`), which also
  accounts for overrides in the pipe cost table."
  (:require [thermos-util.hot-water :as hw]
            [thermos-util :refer [kw->annual-kwh]]
            [thermos-specs.document :as document]))

(defn select-parameters
  "These are all the things in the document which control the pipe
  model, which we want to be able to pick out for efficiency when
  rerendering in the UI.
  "
  [doc]
  (select-keys doc
               [::document/flow-temperature
                ::document/return-temperature
                ::document/ground-temperature
                ::document/pipe-costs]))

(defn power-for-diameter
  "Given a pipe diameter, what power can it carry using the builtin models."
  ^double [doc ^double dia-mm]
  (let [delta-t (document/delta-t doc)
        t-bar   (document/mean-temperature doc)
        dia-m   (/ dia-mm 1000.0)]
    (hw/kw-per-m dia-m delta-t t-bar)))

(defn losses-for-diameter
  "Given a pipe diameter, what are the expected losses in kWh/m.yr"
  [doc dia-mm]
  (kw->annual-kwh
   (/ (hw/heat-loss-w-per-m (document/delta-ground doc) (/ dia-mm 1000.0))
      1000.0)))

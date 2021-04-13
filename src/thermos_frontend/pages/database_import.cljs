;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.pages.database-import
  (:require [thermos-pages.map-import-components :as comps]
            [thermos-frontend.preload :as preload]
            [rum.core :as rum]))

(enable-console-print!)

(defonce state (atom (preload/get-value :initial-state)))

(rum/hydrate
 (comps/map-creation-form state)
 (js/document.getElementById "page-body"))

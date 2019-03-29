(ns thermos-frontend.pages.database-import
  (:require [thermos-pages.map-import-components :as comps]
            [thermos-frontend.preload :as preload]
            [rum.core :as rum]))

(enable-console-print!)

(defonce state (atom (preload/get-value :initial-state)))

(rum/hydrate
 (comps/map-creation-form state)
 (js/document.getElementById "page-body"))

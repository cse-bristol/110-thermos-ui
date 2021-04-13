;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.content-migrations.add-map-view
  (:require [clojure.tools.logging :as log]
            [thermos-backend.content-migrations.core :refer [modify-networks-with]]
            [thermos-specs.document :as document]
            [thermos-specs.defaults :as defaults]
            [thermos-specs.view :as view]
            ))

(defn- fix-parameters [document]
  (assoc-in document
            [::view/view-state ::view/map-view]
            (get-in defaults/default-document [::view/view-state ::view/map-view])))

(defn migrate [conn]
  (log/info "Adding default map-view...")
  (modify-networks-with conn fix-parameters))

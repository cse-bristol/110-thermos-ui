;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.content-migrations.add-supply-model
  (:require [clojure.tools.logging :as log]
            [thermos-backend.content-migrations.core :refer [modify-networks-with]]
            [thermos-specs.defaults :as defaults]))

(defn- add-supply-model [document]
  (merge document defaults/default-supply-model-params))

(defn migrate [conn]
  (log/info "Adding supply model parameters...")
  (modify-networks-with conn add-supply-model))



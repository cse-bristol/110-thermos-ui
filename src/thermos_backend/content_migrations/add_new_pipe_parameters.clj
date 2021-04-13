;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.content-migrations.add-new-pipe-parameters
  (:require [clojure.tools.logging :as log]
            [thermos-backend.content-migrations.core :refer [modify-networks-with]]
            [thermos-specs.document :as document]
            [thermos-specs.defaults :as defaults]))

(defn- fix-parameters [document]
  (merge document
         (select-keys defaults/default-document
                      [::maximum-pipe-diameter
                       ::minimum-pipe-diameter])))

(defn migrate [conn]
  (log/info "Adding default pipe sizes...")
  (modify-networks-with conn fix-parameters))

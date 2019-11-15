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

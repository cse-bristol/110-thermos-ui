(ns thermos-backend.content-migrations.add-supply-model
  (:require [clojure.tools.logging :as log]
            [thermos-backend.content-migrations.core :refer [modify-networks-with]]
            [thermos-specs.defaults :as defaults]))

(defn- add-supply-model [document]
  (merge document defaults/default-supply-model-params))

(defn migrate [conn]
  (log/info "Adding supply model parameters...")
  (modify-networks-with conn add-supply-model))



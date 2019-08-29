(ns thermos-backend.content-migrations.fix-objective-parameters
  (:require [clojure.tools.logging :as log]
            [thermos-backend.content-migrations.core :refer [modify-networks-with]]
            [thermos-specs.document :as document]))

(defn- fix-parameters [document]
  (let [loan-rate (::document/loan-rate document 0)
        loan-term (::document/loan-term document 0)
        new-thing
        {:annualize (not (zero? loan-rate))
         :period loan-term
         :rate loan-rate
         :recur false}]
    (-> document
        (dissoc ::document/loan-term
                ::document/loan-rate)
        (cond-> 
            (not (::document/capital-costs document))
          (assoc ::document/capital-costs
                 {:connection new-thing
                  :insulation new-thing
                  :pipework new-thing
                  :supply new-thing
                  :alternative new-thing})

          (not (::document/objective document))
          (assoc ::document/objective :network)))))


(defn migrate [conn]
  (log/info "Fixing up objective parameters...")

  (modify-networks-with conn fix-parameters))

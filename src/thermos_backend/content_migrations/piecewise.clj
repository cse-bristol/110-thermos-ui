;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.content-migrations.piecewise
  "Migrations that are applied piecewise on content load from the database.

  Previously, all scenarios stored in the database would be updated simultaneously on application start.
  This takes ages and is probably a waste of time since nobody cares about all the old runs anyway.

  Now migrations are applied by db functions when the content is loaded, so the client sees an updated
  problem, which it will save back when the problem gets run.

  To know what migrations a problem needs, we look at ::document/version in it.

  It is important to update the current version in default document,
  so that newly saved stuff doesn't get re-migrated.
  "
  (:require [thermos-specs.document :as document]
            [thermos-backend.content-migrations.tabulate-pipe-parameters
             :refer [tabulate-pipe-parameters]]

            [thermos-backend.content-migrations.user-fields
             :refer [move-user-fields]]))

(def migrations [tabulate-pipe-parameters move-user-fields])
(def current-version (count migrations))

(defn migrate [problem from-version]
  (-> (reduce
       (fn [problem m] (m problem))
       problem
       (drop from-version migrations))))



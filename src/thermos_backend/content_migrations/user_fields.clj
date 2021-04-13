;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.content-migrations.user-fields
  (:require [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [clojure.string :as string]))

(defn move-user-fields [document]
  (document/map-candidates
   document
   (fn [c]
     (let [name (::candidate/name c)
           category (::candidate/subtype c)]
       (-> c
           (assoc ::candidate/user-fields
                  (cond-> {}
                    (not (string/blank? name))
                    (assoc "Name" name)

                    (not (string/blank? category))
                    (assoc "Category" category)))
           (dissoc ::candidate/name ::candidate/subtype))))))

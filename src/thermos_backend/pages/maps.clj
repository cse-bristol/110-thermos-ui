;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.pages.maps
  (:require [thermos-backend.pages.common :refer [page]]
            [thermos-backend.pages.projects :refer [delete-map-form]]))

(defn delete-map-page [map-info]
  (page {:title "Delete map"}
        [:div.card (delete-map-form map-info)]))

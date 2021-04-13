;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-specs.view
  (:require [clojure.spec.alpha :as s]
            [thermos-specs.candidate :as candidate]))

(s/def ::sort-direction #{:asc :desc})

;;TODO Perphaps fill this list with keys from doc/candidate-common
(s/def ::sort-column #{::candidate/type ::candidate/id  ::candidate/postcode})

(s/def ::filters (s/keys :req [::candidate/type ::candidate/selected  ::candidate/postcode]))

;; This is the filter whose pop-up is currently open
(s/def ::open-filter #{::candidate/type ::candidate/id ::candidate/postcode})

(s/def ::table-state (s/keys :req [::sort-column ::sort-direction ::filters ::open-filter]))

(s/def ::bounding-box (s/keys :req-un [::north ::south ::east ::west]))

(s/def ::popover (s/keys :req [::popover-showing ::popover-content ::source-coords]))

(s/def ::map-view #{::constraints ::solution})

(s/def ::show-pipe-diameters boolean?)

(s/def ::view-state (s/keys :req [::table-state ::bounding-box ::popover ::show-forbidden ::map-view
                                  ::show-pipe-diameters]))

(s/def ::show-forbidden boolean?)

(defn switch-to-tab [document tab]
  (assoc-in document [::view-state ::selected-tab] tab))

(defn switch-to-map [document]
  (switch-to-tab document :candidates))

(defn switch-to-tariffs [document]
  (switch-to-tab document :tariffs))

(defn switch-to-pipe-costs [document]
  (switch-to-tab document :pipe-costs))

(defn set-map-view [document view]
  (assoc-in document [::view-state ::map-view] view))

(defn toggle-show-pipe-diameters [document]
  (update-in document [::view-state ::show-pipe-diameters] not))

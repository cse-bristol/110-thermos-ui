(ns thermos-ui.specs.view
  (:require [clojure.spec.alpha :as s]
            [thermos-ui.specs.candidate :as can]))

(s/def ::sort-direction #{:asc :desc})

;;TODO Perphaps fill this list with keys from doc/candidate-common
(s/def ::sort-column #{::can/candidate-type ::can/candidate-id ::can/geometry ::can/name ::can/postcode})

(s/def ::table-state (s/keys :req [::sort-column ::sort-direction ::filters]))

(s/def ::view-state (s/keys :req [::selection ::table-state ::map-state]))

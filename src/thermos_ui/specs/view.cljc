(ns thermos-ui.specs.view
  (:require [clojure.spec.alpha :as s]
            [thermos-ui.specs.candidate :as candidate]))

(s/def ::sort-direction #{:asc :desc})

;;TODO Perphaps fill this list with keys from doc/candidate-common
(s/def ::sort-column #{::candidate/type ::candidate/id  ::candidate/name ::candidate/postcode})

(s/def ::filters (s/keys :req [::candidate/type ::candidate/selected  ::candidate/name ::candidate/postcode]))

;; This is the filter whose pop-up is currently open
(s/def ::open-filter #{::candidate/type ::candidate/id  ::candidate/name ::candidate/postcode})

(s/def ::table-state (s/keys :req [::sort-column ::sort-direction ::filters ::open-filter]))

(s/def ::bounding-box (s/keys :req-un [::north ::south ::east ::west]))

(s/def ::popover (s/keys :req [::popover-showing ::popover-content ::source-coords]))

(s/def ::view-state (s/keys :req [::table-state ::bounding-box ::popover ::show-forbidden]))

(s/def ::show-forbidden boolean?)

(ns thermos-ui.specs.candidate
  (:require [clojure.spec.alpha :as s]
            [thermos-ui.specs.technology :as technology]
            ))

;; at least at the moment, a candidate ID is a string
(s/def ::id string?)

;; a candidate must either be a building or a road for now
(s/def ::candidate
  (s/or :is-a-supply ::supply
        :is-a-demand ::demand
        :is-a-path ::path))

;; This is just to remind us that ::geometry should be geojson but we
;; haven't written a spec for that because it's huge
(defn geojson? [_] true)
(s/def ::geometry geojson?)

;; These are used in the candidate definitions below
(s/def ::length number?)

(s/def ::demand number?) ;; TODO this is not correct; we expect this
                         ;; to include duration information and to ;;
                         ;; have several demands, kamal says: We
                         ;; usually specify demands in power units
                         ;; with a separate parameter indicating the
                         ;; duration of the demand for all the demands
                         ;; specified in a file


(let [postcode-regex
      #"(GIR 0AA)|((([A-Z-[QVX]][0-9][0-9]?)|(([A-Z-[QVX]][A-Z-[IJZ]][0-9][0-9]?)|(([A-Z-[QVX]][0-9][A-HJKPSTUW])|([A-Z-[QVX]][A-Z-[IJZ]][0-9][ABEHMNPRVWXY])))) [0-9][A-Z-[CIKMOV]]{2})"]
  (s/def ::postcode #(re-matches postcode-regex %)))

(s/def ::name string?)
(s/def ::building-type string?)

;; Constraints on candidates

(s/def ::inclusion #{:required :optional :forbidden})
(s/def ::allowed-technologies (s/* ::technology/technology-id))

(s/def ::common
  (s/keys :req [::type ::id ::geometry ::name ::postcode ::inclusion ]))

(s/def ::building
  (s/merge
   ::common
   (s/keys :req [ ::building-type ])))

(s/def ::supply
  (s/and
   #(= :supply (::type %))
   (s/merge
    ::building
    (s/keys :req [ ::allowed-technologies ])
    )))

(s/def ::demand
  (s/and
   #(= :demand (::type %))
   (s/merge
    ::building
    (s/keys :req [ ::demand ]))))

(s/def ::path
  (s/and
   #(= :path (::type %))
   (s/merge
    (s/keys :req [ ::length ])
    ::common)))

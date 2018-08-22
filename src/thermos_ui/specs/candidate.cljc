(ns thermos-ui.specs.candidate
  (:require [clojure.spec.alpha :as s]
            [thermos-ui.specs.technology :as technology]
            [thermos-ui.specs.solution :as solution]
            ))

;; A CANDIDATE is something which can be in a heat network.
;; At the moment there are three types of candidate:

(s/def ::type #{:building :path})

;; - A SUPPLY, which is a place where heat can be produced and injected into a network
;; - A DEMAND, which is a place where heat can be consumed
;; - A PATH, which is a way along which a heat pipe could be installed

(s/def ::candidate
  (s/or :is-a-building ::building
        :is-a-path ::path))

;; All types of candidate have to have certain attributes
(s/def ::common
  (s/keys :req [ ::id ::type ::subtype ::geometry ::name ::inclusion ::selected ]
          :opt [ ::solution/candidate ]
          ))

(s/def ::selected boolean?)

;; Every candidate has a unique ID, which is some text
(s/def ::id string?)

;; Candidates have some geometry, which is geojson. We have not specced
;; What geojson looks like here.
(defn geojson? [_] true)
(s/def ::geometry geojson?)

(s/def ::name string?)

;; Finally, every candidate has a contraint on inclusion in the solutions
;; we generate:

(s/def ::inclusion #{:required :optional :forbidden})

;; Both supplies and demands are defined to exist within BUILDINGs

(s/def ::building (s/and
                   #(= :building (::type %))
                   (s/merge ::common
                            (s/keys :req [ ::connection ]
                                    :opt [ ::allowed-technologies ::demand ]
                                    ))))

(s/def ::connection ::id)

(s/def ::subtype string?)

;; Allowed technologies lists the IDs of technologies that we are
;; considering for this building.
;; TODO we want them to be required / optional I guess?
(s/def ::allowed-technologies (s/map-of ::technology/id
                                        (s/keys :req-un [::min ::max])))

;; DEMAND points are just buildings which have demand information on them:

(s/def ::demand number?) ;; TODO this is not correct; we expect this
                         ;; to include duration information and to ;;
                         ;; have several demands, kamal says: We
                         ;; usually specify demands in power units
                         ;; with a separate parameter indicating the
                         ;; duration of the demand for all the demands
                         ;; specified in a file


;; Finally PATHs; these connect pairs of points, and have length information.

;; The TOPOLOGY of the space where networks can go is inferred by the identities
;; referred to by the start and end of each path. These identities are either for
;; buildings, or they are soley identities for junctions where paths connect.

(s/def ::path
  (s/and
   #(= :path (::type %))
   (s/merge
    (s/keys :req [ ::length ::path-start ::path-end ::path-cost ])
    ::common)))

(s/def ::length number?)
(s/def ::path-start ::id)
(s/def ::path-end ::id)

;; access / update functions
(defn is-included? [{inc ::inclusion}]
  (or (= inc :optional)
      (= inc :required)))

(defn is-required? [{inc ::inclusion}]
  (= inc :required))

(defn is-excluded? [c]
  (not (is-included? c)))

(defn is-building? [{t ::type}]
  (= t :building))

(defn is-demand? [{d ::demand t ::type}]
  (and (= t :building) d))

(defn is-path? [{t ::type}]
  (= t :path))

(defn is-supply? [{s ::allowed-technologies t ::type}]
  (and (= t :building)
       (some #(> (:max %) 0) (vals s))))

(defn annual-demand [{d ::demand}] d)

(defn forbid-supply [c]
  (assoc c ::allowed-technologies {}))


(ns thermos-ui.frontend.operations
  (:require [clojure.spec.alpha :as s]
            [thermos-ui.specs.document :as document]
            [thermos-ui.specs.candidate :as candidate]
            )
  )

(s/def ::selection-method
  #{:replace :union :intersection :difference :xor})

(defn selected-candidates-ids
  "Get a set containing the candidate IDs of all the selected candidates in the doc"
  [doc]

  (->> doc
       (::document/candidates)
       (vals)
       (filter ::candidate/selected)
       (map ::candidate/id)
       (set)))

(defn all-candidates-ids
  "Get a lazy sequence containing the ID of each candidate in doc"
  [doc]
  (->> doc
       (::document/candidates)
       (vals)
       (map ::candidate/id)))

(defn select-candidates
  "Change the selection for all candidates with IDs in the CANDIDATE-IDS using the METHOD."
  [doc candidate-ids method]
  (let [candidate-ids (set candidate-ids)

        test-fn
        (case method
          :replace
          (fn [_ s] s)

          :union
          (fn [d s] (or d s))

          :intersection
          (fn [d s] (and d s))

          :difference
          (fn [d s] (and d (not s)))
          :xor
          (fn [d s] (and (or d s) (not (and d s)))))

        update-fn
        (fn [candidate]
          (let [is-selected-now
                (boolean
                 (test-fn (::candidate/selected candidate)
                          (candidate-ids (::candidate/id candidate))))]

            (assoc candidate ::candidate/selected is-selected-now)))
        ]

    (update doc ::document/candidates
            (fn [candidates]
              (reduce (fn [candidates id]
                        (update candidates id update-fn))
                      candidates (keys candidates))))))

(defn select-all-candidates [doc]
  (select-candidates doc (all-candidates-ids doc) :replace))

(defn map-candidates
  "Go through a document and apply f to all the indicated candidates."
  ([doc f]
   (map-candidates f (all-candidates-ids doc)))

  ([doc f ids]
   (let [candidates (::document/candidates doc)]
     (assoc doc ::document/candidates
            (reduce
             (fn [id] (update doc id f))
             doc ids)))))

(defn set-candidates-inclusion
  "Change the inclusion constraint for candidates in CANDIDATE-IDS to NEW-CONSTRAINT"
  [doc candidate-ids new-constraint]
  (map-candidates doc
                  #(assoc % ::candidate/constraint new-constraint)
                  candidate-ids))


(defn set-map-position
  "Show a particular boundingbox on the map"
  [doc bbox])

(defn set-map-colouring
  "Change the colour scheme for the map"
  [doc scheme])


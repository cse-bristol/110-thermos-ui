(ns thermos-ui.frontend.operations
  (:require [clojure.spec.alpha :as s]
            [thermos-ui.specs.document :as document]
            [thermos-ui.specs.candidate :as candidate]
            [thermos-ui.specs.view :as view]
            ))

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

(defn selected-candidates
  "Get a set containing all the selected candidates in the doc"
  [doc]
  (->> doc
       (::document/candidates)
       (vals)
       (filter ::candidate/selected)))

(defn- included [{inclusion ::candidate/inclusion}]
  (or (= :optional inclusion)
      (= :required inclusion)))

(defn included-candidates
  "Get a set containing all the selected candidates in the doc"
  [doc]
  (->> doc
       (::document/candidates)
       (vals)
       (filter included)))

(defn all-candidates-ids
  "Get a lazy sequence containing the ID of each candidate in doc"
  [doc]
  (->> doc
       (::document/candidates)
       (vals)
       (map ::candidate/id)))

(defn constrained-candidates-ids
  "Get a set containing the candidate IDS of all the candidates that have a constraint."
  [doc]
  (->> doc
       (::document/candidates)
       (vals)
       (filter ::candidate/inclusion)
       (map ::candidate/id)
       (set)))

(defn constrained-candidates
  "Get a collection containing all the candidates that have a constraint."
  [doc]
  (->> doc
       (::document/candidates)
       (vals)
       (filter ::candidate/inclusion)))

(defn map-candidates
  "Go through a document and apply f to all the indicated candidates."
  [doc f & [ids]]
  (let [ids (or ids (all-candidates-ids doc))]
    (update doc
            ::document/candidates
            #(reduce
              (fn [cands id] (update cands id f))
              % ids))))

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
              (persistent!
               (reduce (fn [candidates id]
                         (assoc! candidates id (update-fn (get candidates id))))
                       (transient candidates)
                       (keys candidates)))))))

(defn select-all-candidates [doc]
  (select-candidates doc (all-candidates-ids doc) :replace))

(defn set-candidates-inclusion
  "Change the inclusion constraint for candidates in CANDIDATE-IDS to NEW-CONSTRAINT"
  [doc candidate-ids new-constraint]
  (map-candidates doc
                  #(assoc % ::candidate/inclusion new-constraint)
                  candidate-ids))

(defn move-map
  "Show a particular boundingbox on the map"
  [doc bbox]
  (assoc-in doc [::view/view-state ::view/bounding-box]
            bbox))

(defn set-map-colouring
  "Change the colour scheme for the map"
  [doc scheme])

(defn insert-candidates
  "Insert some candidates into a document map, but preserve
  candidates which have user changes on them"
  [document new-candidates]

  (update
   document
   ::document/candidates
   (fn [current-candidates]
     (persistent!
      (reduce
       (fn [candidates new-candidate]
         (let [candidate-id (::candidate/id new-candidate)]
           (if (get candidates candidate-id) ;; look up new candidate's ID
             ;; in the existing candidates.
             candidates ;; If it already exists, don't change anything
             (assoc! candidates candidate-id new-candidate))))
       (transient (or current-candidates {}))
       new-candidates)
      ))))

(defn deselect-candidates
  "Removes the given candidates from the current selection."
  [document candidate-ids]
  (map-candidates document
                  #(assoc % ::candidate/selected false)
                  candidate-ids))

(defn load-document
  "Load a new document, but keep useful stuff from old-document"
  [old-document new-document]
  (update new-document
          ::document/candidates
          #(merge (::document/candidates old-document) %)))

(defn show-popover
  [document]
  (assoc-in document [::view/view-state ::view/popover ::view/showing] true))

(defn hide-popover
  [document]
  (assoc-in document [::view/view-state ::view/popover ::view/showing] false))

(defn set-popover-content
  [document content]
  (assoc-in document [::view/view-state ::view/popover ::view/popover-content] content))

(defn set-popover-source-coords
  [document source-coords]
  (assoc-in document [::view/view-state ::view/popover ::view/source-coords] source-coords))

(defn close-popover
  [document]
  ((comp hide-popover (fn [doc] (set-popover-content doc nil))) document))

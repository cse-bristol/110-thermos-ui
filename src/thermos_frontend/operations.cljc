;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.operations
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]

            [com.rpl.specter :as x :refer-macros [select transform]]
            
            [thermos-specs.document :as document]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.solution :as solution]
            [thermos-specs.view :as view]))

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
       (filter #(> (.indexOf [:required :optional] (::candidate/inclusion %)) -1))
       (map ::candidate/id)
       (set)))

(defn constrained-candidates
  "Get a collection containing all the candidates that have a constraint."
  [doc]
  (->> doc
       (::document/candidates)
       (vals)
       (filter #(> (.indexOf [:required :optional] (::candidate/inclusion %)) -1))))

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

    (x/transform
     [::document/candidates x/MAP-VALS]
     update-fn
     doc
     )))

(defn select-all-candidates [doc]
  (select-candidates doc (all-candidates-ids doc) :replace))

(defn set-candidates-inclusion
  "Change the inclusion constraint for candidates in CANDIDATE-IDS to NEW-CONSTRAINT"
  [doc candidate-ids new-constraint]
  (document/map-candidates doc
                           #(assoc % ::candidate/inclusion new-constraint)
                           candidate-ids))

(defn rotate-candidates-inclusion [doc candidate-ids]
  (if (empty? candidate-ids)
    doc
    (let [inclusion (get-in doc [::document/candidates
                                 (first candidate-ids)
                                 ::candidate/inclusion])]
      (set-candidates-inclusion doc
                                candidate-ids
                                (case inclusion
                                  :forbidden :optional
                                  :optional :required
                                  :required :forbidden

                                  :optional)))))

(defn move-map
  "Show a particular boundingbox on the map"
  [doc bbox]
  (assoc-in doc [::view/view-state ::view/bounding-box]
            bbox))

(defn set-map-colouring
  "Change the colour scheme for the map"
  [doc scheme])

(defn insert-candidates
  "Insert some candidates into a document map, but preserve candidates
  which have user changes on them `new-candidates` should pairs of
  candidate ID to a candidate or a delay - we will use force on it if
  we want to keep it.
  "
  [document new-candidates]
  
  (let [deletions (set (::document/deletions document))]
    (update
     document
     ::document/candidates
     (fn [current-candidates]
       (persistent!
        (reduce
         (fn [candidates [candidate-id new-candidate]]
           (cond-> candidates
             (not (or (contains? deletions candidate-id)
                      (get candidates candidate-id)))
             (assoc! candidate-id (force new-candidate))))
         (transient (or current-candidates {}))
         new-candidates
         ))))))

(defn deselect-candidates
  "Removes the given candidates from the current selection."
  [document candidate-ids]
  (document/map-candidates document
                  #(assoc % ::candidate/selected false)
                  candidate-ids))

(defn load-document
  "Load a new document, but keep useful stuff from old-document"
  [old-document new-document]
  (update new-document
          ::document/candidates
          #(merge (::document/candidates old-document) %)))

;; @TODO
;; The below 7 are all UI things, so maybe they should go somewhere else.

(defn close-table-filter
  "Close the currently open filter pop-up."
  [document]
  (assoc-in document [::view/view-state ::view/table-state ::view/open-filter] nil))

(defn open-table-filter
  "Open a table filter pop-up.
  `filter-key` should be the key for the property you want to filter by, e.g. ::candidate/postcode"
  [document filter-key]
  (assoc-in document [::view/view-state ::view/table-state ::view/open-filter] filter-key))

(defn table-filter-open? [document]
  (get-in document [::view/view-state ::view/table-state ::view/open-filter]))

(defn get-table-filters
  "Fetch the selected filters for the given `filter-key`, e.g. ::candidate/postcode."
  [document filter-key]
  (get-in document
          [::view/view-state
           ::view/table-state
           ::view/filters
           filter-key]))


(defn get-all-table-filters
  "Fetch the selected filters for all filterable properties."
  [document]
  (get-in document
          [::view/view-state
           ::view/table-state
           ::view/filters]))

(defn set-table-filter-value
  "Add a filter value to the table filter for a given candidate property.
  `filter-key` should be the key for the property you want to filter by, e.g. ::candidate/postcode
  `value` should be the value you want to add to the filter, e.g. BS1 234 if filtering by postcode."
  [document filter-key value]
  (let [current-filter-set (or (get-table-filters document filter-key) #{})]
    (assoc-in document
              [::view/view-state ::view/table-state ::view/filters filter-key] value)))

(defn add-table-filter-value
  "Add a filter value to the table filter for a given candidate property.
  `filter-key` should be the key for the property you want to filter by, e.g. ::candidate/postcode
  `value` should be the value you want to add to the filter, e.g. BS1 234 if filtering by postcode."
  [document filter-key value]
  (let [current-filter-set (or (get-table-filters document filter-key) #{})]
    ;; Default case, for the other fields which are all checkbox filters
    (assoc-in document
              [::view/view-state ::view/table-state ::view/filters filter-key]
              (conj current-filter-set value))))

(defn add-table-filter-values
  "As above but with set of values to add."
  [document filter-key values]
  (let [current-filter-set (or (get-table-filters document filter-key) #{})]
    (case filter-key
      (assoc-in document
                [::view/view-state ::view/table-state ::view/filters filter-key]
                (apply conj current-filter-set values)))))

(defn remove-table-filter-value
  "Remove a filter value from the table filter for a given candidate property.
  `filter-key` should be the key for the property you want to filter by, e.g. ::candidate/postcode
  `value` should be the value you want to remove from the filter, e.g. BS1 234 if filtering by postcode."
  [document filter-key value]
  (let [current-filter-set (or (get-table-filters document filter-key) #{})]
    ;; Default case, for the other fields which are all checkbox filters
    (let [existing-value (get-in document [::view/view-state ::view/table-state ::view/filters filter-key])
          new-value (if (false? value)
                      (remove false? existing-value)
                      (remove #{value} existing-value))
          ]
      (if (empty? new-value)
        (update-in document [::view/view-state ::view/table-state ::view/filters] dissoc filter-key)
        (assoc-in document [::view/view-state ::view/table-state ::view/filters filter-key] new-value)
        )
      )))

(defn clear-filters
  [document]
  (assoc-in document [::view/view-state ::view/table-state ::view/filters] nil))

(defn remove-all-table-filter-values
  [document filter-key]
  (update-in document [::view/view-state ::view/table-state ::view/filters] dissoc filter-key))

(defn get-filtered-candidates
  "Returns all candidates which are constrained and meet the filter criteria."
  [doc]
  (let [items (constrained-candidates doc)
        filters (get-all-table-filters doc)]
    
    (if (empty? filters) items
        (let [filter->fn
              (fn [[key value]]
                (let [test-value
                      (cond
                        (vector? value)
                        (let [[lb ub] value]
                          #(and (number? %)
                                (<= lb %)
                                (<= % ub)))
                        
                        (string? value)
                        (let [pattern (re-pattern (str "(?i)" (str/join ".*" (str/split value #"")) ".*"))]
                          #(and % (re-find pattern %)))

                        (and (set? value) (not-empty value))
                        value
                        
                        :otherwise
                        (constantly true))

                      get-value
                      (if (seqable? key)
                        #(get-in % key)
                        #(get % key))]
                  
                  #(test-value (get-value %))))
              
              filter-fns
              (map filter->fn filters)

              overall-filter
              (apply every-pred filter-fns)]

          (filter overall-filter items)
          ))))

(defn showing-forbidden?
  [doc]
  (->> doc ::view/view-state ::view/hide-forbidden not))

(defn toggle-showing-forbidden [doc]
  (let [doc (update-in doc [::view/view-state ::view/hide-forbidden] not)]
    (if (showing-forbidden? doc)
      doc
      (update-in doc [::document/candidates]
                 #(select-keys % (constrained-candidates-ids doc))))))

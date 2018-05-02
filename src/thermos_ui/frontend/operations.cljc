(ns thermos-ui.frontend.operations
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
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

;; @TODO
;; The below 7 are all UI things, so maybe they should go somewhere else.

(defn show-popover
  [document]
  (assoc-in document [::view/view-state ::view/popover ::view/popover-showing] true))

(defn hide-popover
  [document]
  (assoc-in document [::view/view-state ::view/popover ::view/popover-showing] false))

(defn set-popover-content
  [document content]
  (assoc-in document [::view/view-state ::view/popover ::view/popover-content] content))

(defn set-popover-source-coords
  [document source-coords]
  (assoc-in document [::view/view-state ::view/popover ::view/source-coords] source-coords))

(defn close-popover
  [document]
  ((comp hide-popover (fn [doc] (set-popover-content doc nil))) document))

(defn close-table-filter
  "Close the currently open filter pop-up."
  [document]
  (assoc-in document [::view/view-state ::view/table-state ::view/open-filter] nil))

(defn open-table-filter
  "Open a table filter pop-up.
  `filter-key` should be the key for the property you want to filter by, e.g. ::candidate/postcode"
  [document filter-key]
    (assoc-in document [::view/view-state ::view/table-state ::view/open-filter] filter-key))

(defn get-table-filters
  "Fetch the selected filters for the given `filter-key`, e.g. ::candidate/postcode."
  [document filter-key]
  (->> document
       ::view/view-state
       ::view/table-state
       ::view/filters
       filter-key))

(defn get-all-table-filters
  "Fetch the selected filters for all filterable properties."
  [document]
  (->> document
       ::view/view-state
       ::view/table-state
       ::view/filters))

(defn add-table-filter-value
  "Add a filter value to the table filter for a given candidate property.
  `filter-key` should be the key for the property you want to filter by, e.g. ::candidate/postcode
  `value` should be the value you want to add to the filter, e.g. BS1 234 if filtering by postcode."
  [document filter-key value]
  (let [current-filter-set (or (get-table-filters document filter-key) #{})]
    (case filter-key
      ;; In the case of 'name' we are doing a text search, so just replace the value with the new string
      ::candidate/name
      (assoc-in document
                [::view/view-state ::view/table-state ::view/filters filter-key] value)
      ;; Default case, for the other fields which are all checkbox filters
      (assoc-in document
                [::view/view-state ::view/table-state ::view/filters filter-key]
                (conj current-filter-set value)))))

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
    (case filter-key
      ;; For `name` field just set the filter value to nil
      ::candidate/name
      (assoc-in document
                [::view/view-state ::view/table-state ::view/filters filter-key] nil)
      ;; Default case, for the other fields which are all checkbox filters
      (assoc-in document
                [::view/view-state ::view/table-state ::view/filters filter-key]
                (set (if (false? value) ;; (remove #{value} current-filter-set) doesn't work when value=false
                       (remove false? current-filter-set)
                       (remove #{value} current-filter-set)))))))

(defn remove-all-table-filter-values
  [document filter-key]
  (case filter-key
    ;; For `name` field just set the filter value to nil
    ::candidate/name
    (assoc-in document
              [::view/view-state ::view/table-state ::view/filters filter-key] nil)
    ;; Default case, for the other fields which are all checkbox filters
    (assoc-in document
              [::view/view-state ::view/table-state ::view/filters filter-key]
              #{})))

(defn set-splitpane
  "It will be useful for us to have a reference to the splitpane as a goog.Component
  which we have access to globally, so set it here when the splitpane is created."
  [document splitpane]
  (assoc-in document [::view/view-state ::view/splitpane] splitpane))

(defn show-toaster
  [doc]
  (assoc-in doc [::view/view-state ::view/toaster ::view/toaster-showing] true))

(defn hide-toaster
  [doc]
  (assoc-in doc [::view/view-state ::view/toaster ::view/toaster-showing] false))

(defn set-toaster-content
  [doc content]
  (assoc-in doc [::view/view-state ::view/toaster ::view/toaster-content] content))

(defn set-toaster-class
  [doc class]
  (assoc-in doc [::view/view-state ::view/toaster ::view/toaster-class] class))

(defn get-filtered-candidates
  "Returns all candidates which are constrained and meet the filter criteria."
  [doc]
  (let [items (constrained-candidates doc)
        filters (get-all-table-filters doc)]
    (if (not-empty filters)
      (filter
       (fn [item]
         (reduce-kv
          (fn [init k v]
            (if init
              (if (string? v)
                ;; If the filter is a string then check if the item matches the string in a fuzzy way.
                (let [regex-pattern (re-pattern (str "(?i)" (str/join ".*" (str/split v #"")) ".*"))]
                  (not-empty
                   (re-seq regex-pattern (k item))))
                ;; If the filter is a set of values the check if the item matches one of them exactly.
                (or (empty? v) (contains? v (k item)))
                )
              false))
          true
          filters))
       items)
      items)))

(defn showing-forbidden?
  [doc]
  (->> doc ::view/view-state ::view/show-forbidden))

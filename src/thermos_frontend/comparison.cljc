;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.comparison
  "Comparing the network being edited (`here`) with another saved network
  (`there`), for the editor's comparison mode: how each candidate differs,
  which the map colours by its `category`, how the summary figures
  differ, and which parameters differ.

  A saved network only has its interesting candidates (see
  document/keep-interesting), and the editor only loads the others around
  where the map is, so a candidate which a document doesn't have is taken
  to be as it is in the map: forbidden, unchanged and in no solution. One
  which a document has deleted (see ::document/deletions) isn't there at
  all."
  (:require [clojure.string :as string]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.document :as document]
            [thermos-specs.parameters :as parameters]
            [thermos-specs.solution :as solution]
            [thermos-util.network-summary :as network-summary]
            [thermos-util.solution-summary :as solution-summary]))

;; Candidates

(defn- on-network? [c]
  (boolean (or (candidate/is-connected? c) (candidate/supply-in-solution? c))))

(def ^:private solution-ns (namespace ::solution/included))

(def ^:private not-inputs
  "Candidate keys which aren't compared as inputs: those which the editor
  keeps for itself, those compared separately, and the geometry, which is
  the same for the same id but may be kept as json or as a js object"
  #{::candidate/selected ::candidate/modified ::candidate/inclusion ::candidate/geometry})

(defn- inputs
  "The parts of candidate `c` which go into the problem, other than its
  inclusion"
  [c]
  (into {} (remove (fn [[k _]] (or (contains? not-inputs k)
                                   (document/is-transient-key? k)
                                   (= solution-ns (namespace k)))))
        c))

(defn category
  "Which way the map shows a `candidate-diff`:
  :added     on the network (or there at all) only here
  :removed   on the network (or there at all) only there
  :changed   otherwise different
  :same      on both networks, and the same
  :unchanged on neither network, and the same"
  [{:keys [presence network inclusion system diameter edited]}]
  (cond
    (or (= :here presence) (= :here network))   :added
    (or (= :there presence) (= :there network)) :removed
    (or inclusion system diameter edited)       :changed
    (= :both network)                           :same
    :else                                       :unchanged))

(defn candidate-diff
  "How candidate `a`, here, differs from `b`, there. Either is nil if that
  document doesn't have the candidate, so it is as in the map, or :deleted
  if that document has deleted it. Heating systems are compared only if
  `systems?`, which is when both documents have a solution; otherwise
  the network is enough. Has the candidate's :id and :type, and
  :category, and only the differences of

  :presence  :here or :there, if it is only in one document
  :network   :here, :there or :both, if it is on either network
  :inclusion [here there], if its constraint changed
  :system    [here there], if its heating system (see candidate/system-name)
             changed
  :diameter  [here there] in mm, for a path on both networks
  :edited    true, if any of its other inputs, such as its demand or
             tariff, changed"
  [a b systems?]
  (let [present-a (not= :deleted a)
        present-b (not= :deleted b)
        a (when present-a a)
        b (when present-b b)
        c (or a b)
        net-a (on-network? a)
        net-b (on-network? b)
        inc-a (::candidate/inclusion a :forbidden)
        inc-b (::candidate/inclusion b :forbidden)
        sys-a (candidate/system-name a)
        sys-b (candidate/system-name b)
        dia-a (::solution/diameter-mm a)
        dia-b (::solution/diameter-mm b)
        diff (cond-> {:id (::candidate/id c) :type (::candidate/type c)}
               (not= present-a present-b)
               (assoc :presence (if present-a :here :there))

               (or net-a net-b)
               (assoc :network (cond (and net-a net-b) :both net-a :here :else :there))

               (and present-a present-b (not= inc-a inc-b))
               (assoc :inclusion [inc-a inc-b])

               (and systems? present-a present-b (candidate/is-building? c) (not= sys-a sys-b))
               (assoc :system [sys-a sys-b])

               (and net-a net-b (not= dia-a dia-b))
               (assoc :diameter [dia-a dia-b])

               (and a b (not= (inputs a) (inputs b)))
               (assoc :edited true))]
    (assoc diff :category (category diff))))

(defn- candidate-in
  "Candidate `id` in `doc` as `candidate-diff` takes it, given the
  document's candidates and a set of its deletions"
  [candidates deletions id]
  (if (contains? deletions id) :deleted (get candidates id)))

(defn diff
  "How the candidates in documents `here` and `there` differ, as a map
  from id to `candidate-diff`, for each candidate which either document
  saves, or deletes while the other has it"
  [here there]
  (let [ch (::document/candidates here)
        ct (::document/candidates there)
        dh (set (::document/deletions here))
        dt (set (::document/deletions there))
        systems? (and (document/has-solution? here) (document/has-solution? there))
        interesting (comp (filter (comp document/is-interesting? val)) (map key))
        ids (-> #{}
                (into interesting ch)
                (into interesting ct)
                (into (filter #(contains? ct %)) dh)
                (into (filter #(contains? ch %)) dt))]
    (persistent!
     (reduce
      (fn [m id]
        (assoc! m id (candidate-diff (candidate-in ch dh id) (candidate-in ct dt id) systems?)))
      (transient {})
      ids))))

(defn category-counts
  "How many candidates of each type are in each category of `diff`, as
  {[type category] n}"
  [diff]
  (frequencies (map (juxt :type :category) (vals diff))))

(defn candidate-rows
  "Facts about candidate `a` here and `b` there (either nil if that
  document doesn't have it, or :deleted), for showing side by side, as
  [{:label :unit :scale :here :there}]. Only the rows which either has a
  value for."
  [a b mode]
  (let [a (when (map? a) a)
        b (when (map? b) b)
        rows [{:label "Constraint"
               :value #(when % (name (::candidate/inclusion % :forbidden)))}
              {:label "In solution"   :value candidate/solution-description}
              {:label "Demand"        :value #(candidate/solved-annual-demand % mode)
               :unit "Wh/yr" :scale 1000}
              {:label "Peak"          :value #(candidate/solved-peak-demand % mode)
               :unit "Wp" :scale 1000}
              {:label "Diameter"      :value ::solution/diameter-mm :unit "mm"}
              {:label "Capacity"      :value ::solution/capacity-kw :unit "W" :scale 1000}
              {:label "Supply output" :value ::solution/output-kwh :unit "Wh/yr" :scale 1000}
              {:label "Pipe cost"     :value (comp :principal ::solution/pipe-capex) :unit "¤"}
              {:label "Connection cost" :value (comp :principal ::solution/connection-capex)
               :unit "¤"}
              {:label "Individual system cost"
               :value (comp :principal :capex ::solution/alternative) :unit "¤"}
              {:label "Revenue"       :value (comp :annual ::solution/heat-revenue) :unit "¤/yr"}]]
    (vec
     (for [{:keys [value] :as row} rows
           :let [here  (when a (value a))
                 there (when b (value b))]
           :when (or (some? here) (some? there))]
       (-> row (dissoc :value) (assoc :here here :there there))))))

;; Summary figures

(defn- saved-candidates-only
  "`doc` with only the candidates it would be saved with, so that the
  ones the editor has loaded around the map don't count"
  [doc]
  (update doc ::document/candidates
          #(into {} (filter (comp document/is-interesting? val)) %)))

(defn summary
  "The figures to compare for `doc`: those in its metadata (see
  network-summary/summarise), with the supply capacity it could have, and
  its whole system costs from the solution summary"
  [doc]
  (let [doc     (saved-candidates-only doc)
        summary (network-summary/summarise doc)
        costs   (when (solution/exists? doc)
                  (:grand-total (solution-summary/data-table doc :principal :annual)))]
    (cond-> (assoc summary :supply-capacity
                   (reduce + 0 (filter number? (:supply-capacities summary))))
      costs (assoc :capex (:capex costs)
                   :opex (:opex costs)
                   :present (:present costs)))))

(defn figures
  "The figures `summary-rows` compares, for a network in `mode`"
  [mode]
  (let [heat (if (= :cooling mode) "Cold" "Heat")]
    [{:key :objective-value      :label "Objective value"}
     {:key :network-length       :label "Network length" :unit "m"}
     {:key :network-count        :label "Buildings connected"}
     {:key :network-kwh          :label (str heat " delivered") :unit "Wh/yr" :scale 1000}
     {:key :network-kwp          :label "Supply capacity used" :unit "Wp" :scale 1000}
     {:key :capex                :label "Capital cost" :unit "¤"}
     {:key :opex                 :label "Operating cost" :unit "¤/yr"}
     {:key :present              :label "Whole system NPV" :unit "¤"}
     {:key :input-building-count :label "Buildings in problem"}
     {:key :input-building-kwh   :label (str heat " demand in problem") :unit "Wh/yr" :scale 1000}
     {:key :input-path-length    :label "Paths in problem" :unit "m"}
     {:key :supply-capacity      :label "Supply capacity available" :unit "Wp" :scale 1000}
     {:key :runtime              :label "Runtime" :unit "s"}]))

(defn summary-rows
  "The figures from `summary` for both documents side by side, as
  [{:key :label :unit :scale :here :there :delta}], where :delta is here
  less there, if both are numbers"
  [here-summary there-summary mode]
  (vec
   (for [{k :key :as figure} (figures mode)
         :let [h (get here-summary k)
               t (get there-summary k)]]
     (assoc figure
            :here h
            :there t
            :delta (when (and (number? h) (number? t)) (- h t))))))

;; Tables of figures, as in the solution summary

(defn- merged-keys
  "The keys `a`, in order, with any only in `b` after the key before them
  in `b`, so that a row only in one table stays near its neighbours
  rather than after the totals"
  [a b]
  (reduce (fn [ks [prev k]]
            (if (some #{k} ks)
              ks
              (let [i (if (nil? prev) 0 (inc (.indexOf ks prev)))]
                (-> (subvec ks 0 i) (conj k) (into (subvec ks i))))))
          (vec (distinct a))
          (map vector (cons nil b) b)))

(defn table-rows
  "Matches up the rows of a table of figures for `here` and `there`, each
  [{:key :values ...}] with :values in the order of `columns`, by :key.
  Each row is here's (or else there's) without :values, with :here and
  :there, the values from each side (nil if it doesn't have the row), and
  :delta, here less there for each value which is a number on both sides.
  A side without the row counts as 0 in a column which sums things (any
  not marked :sum? false), so that a pipe size used by only one network
  still has a change."
  [columns here there]
  (let [by-key (fn [rows] (into {} (map (juxt :key identity)) rows))
        h-rows (by-key here)
        t-rows (by-key there)]
    (vec
     (for [k (merged-keys (map :key here) (map :key there))
           :let [h (get h-rows k)
                 t (get t-rows k)
                 hv (when h (vec (:values h)))
                 tv (when t (vec (:values t)))]]
       (-> (or h t)
           (dissoc :values)
           (assoc :here hv
                  :there tv
                  :delta (vec
                          (for [[i {:keys [sum?] :or {sum? true}}] (map-indexed vector columns)
                                :let [zero (when sum? 0)
                                      a (if h (get hv i) zero)
                                      b (if t (get tv i) zero)]]
                            (when (and (number? a) (number? b)) (- a b))))))))))

;; Parameters

(def ^:private parameter-labels
  {::document/npv-term "NPV term"
   ::document/npv-rate "NPV rate"
   ::document/mip-gap "MIP gap"
   ::document/param-gap "Parameter gap"
   ::document/maximum-supply-sites "Maximum supply sites"
   ::document/pipe-costs "Pipe costs"})

(defn parameter-label [k]
  (or (parameter-labels k)
      (string/capitalize (string/replace (name k) "-" " "))))

(defn- scalar? [x]
  (or (nil? x) (number? x) (string? x) (keyword? x) (boolean? x)))

(defn- describe-table-change
  "What differs between two tables of things with names, such as tariffs:
  which names are only in one, and which are different"
  [{name-key :name id-key :id} a b]
  (let [by-name (fn [t] (into {} (for [row (vals t)]
                                   [(get row name-key) (dissoc row id-key)])))
        a (by-name a)
        b (by-name b)
        only    (fn [x y] (sort (remove #(contains? y %) (keys x))))
        changed (sort (filter #(and (contains? b %) (not= (get a %) (get b %))) (keys a)))]
    (->> [(when-let [x (seq (only a b))] (str "only here: " (string/join ", " x)))
          (when-let [x (seq (only b a))] (str "only there: " (string/join ", " x)))
          (when (seq changed) (str "changed: " (string/join ", " changed)))]
         (remove nil?)
         (string/join "; "))))

(defn- describe-pipe-costs-change [a b]
  (let [ra (:rows a) rb (:rows b)
        only (fn [x y] (sort (remove #(contains? y %) (keys x))))
        changed (filter #(and (contains? rb %) (not= (get ra %) (get rb %))) (keys ra))]
    (->> [(when-let [x (seq (only ra rb))] (str "sizes only here: " (string/join ", " x) " mm"))
          (when-let [x (seq (only rb ra))] (str "sizes only there: " (string/join ", " x) " mm"))
          (when (seq changed) (str "costs changed for " (count changed) " sizes"))
          (when (not= (:civils a) (:civils b)) "civil costs changed")
          (when (not= (:default-civils a) (:default-civils b)) "default civils changed")]
         (remove nil?)
         (string/join "; "))))

(defn- not-empty-or-changed
  "A description of a change, or just \"changed\" if it has none, as when
  only ids differ"
  [s]
  (if (string/blank? s) "changed" s))

(defn parameter-differences
  "The parameters which differ between `here` and `there`, by the
  categories of parameter sets, as [{:category :key :label :here :there}]
  for a single value, or [{:category :key :label :detail}] for a table of
  them, with :detail saying what differs"
  [here there]
  (vec
   (for [c parameters/categories
         :let [table (parameters/category-tables c)]
         k (parameters/keys-for-category c)
         :let [a (get here k) b (get there k)]
         :when (not= a b)]
     (merge {:category c :key k :label (parameter-label k)}
            (cond
              (and (scalar? a) (scalar? b))
              {:here a :there b}

              (= k (:table table))
              {:detail (not-empty-or-changed (describe-table-change table a b))}

              (= k ::document/pipe-costs)
              {:detail (not-empty-or-changed (describe-pipe-costs-change a b))}

              :else
              {:detail "changed"})))))

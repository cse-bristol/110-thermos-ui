(ns thermos-cli.zone-cli-rules
  (:require [thermos-specs.demand :as demand]
            [thermos-specs.candidate :as candidate]
            [clojure.string :as string]))

(defn matches-rule?
  "Horrible rule engine
   options for rules are

  - :default => always matches
  - [:and | :or | :not & rules] => obvious
  - [:in FIELD X1 X2 X3]
  - [:demand< | :peak< X]
  - [:demand> | :peak> X]
  "
  [candidate rule]
  (cond
    (= :default rule) true

    (= :mandatable rule) (and (candidate/is-building? candidate)
                              (:mandatable? candidate false))

    (= :infill rule) (and (candidate/is-building? candidate)
                          (not (:mandatable? candidate false)))
    
    (= :building rule)   (candidate/is-building? candidate)
    (= :path rule)       (candidate/is-path? candidate)
    
    :else
    (let [[op & args] rule]
      (case op
        :and     (every? #(matches-rule? candidate %) args)
        :or      (some #(matches-rule? candidate %) args)
        :not     (not (matches-rule? candidate (first args)))
        :in      (let [[field & values] args]
                   (contains? (set values) (get candidate field)))
        :is      (let [[field value] args
                       value (get candidate field)]
                   (or (and (boolean? value) value)
                       (and (integer? value) (= 1 value))
                       (and (double? value) (= 1.0 value))
                       (and (string? value)
                            (let [lc (string/lower-case value)]
                              (or (= lc "yes")
                                  (= lc "true")
                                  (= lc "y")
                                  (= lc "1"))))))
        (:demand< :peak<)
        (and
         (candidate/is-building? candidate)
         (let [threshold (first args)
               x         (get candidate
                              (if (= op :demand<) ::demand/kwh ::demand/kwp))]
           (< x threshold)))

        (:demand> :peak>)
        (and
         (candidate/is-building? candidate)
         (let [threshold (first args)
               x         (get candidate
                              (if (= op :demand>) ::demand/kwh ::demand/kwp))]
           (> x threshold)))

        false))))

(defn matching-rule [candidate rules]
  (first (filter (fn [[rule value]] (matches-rule? candidate rule)) rules)))

(defn assign-matching-value [candidate rules key]
  (let [rule (matching-rule candidate rules)]
    (cond-> candidate
      rule (assoc key (second rule)))))

(defn all-matching-rules [candidate rules]
  (let [rules (filter (fn [[rule value]] (matches-rule? candidate rule)) rules)
        stop-at? (conj (map #(= (get % 2) :next) rules) 
                       true)]
    (->> rules
         (map vector stop-at?)
         (take-while (fn [[stop?]] stop?))
         (map second))))

(defn assign-all-matching-values
  "If the 3rd element in a matching rule is the keyword `:next`, continue to evaluate
   subsequent rules. 
   
   Values from all matching rules are combined into a set."
  [candidate rules key]
  (let [rules (all-matching-rules candidate rules)]
    (cond-> candidate
      (seq rules) (assoc key (->> rules
                                  (map second)
                                  (flatten)
                                  (set))))))

(comment
  (let [rules [[[:in "cost_category" "soft"] 1] [[:in "cost_category" "city-centre"] 2] [[:in "cost_category" "motorway"] 3] [[:in "cost_category" "non-city-centre"] 4] [[:in "cost_category" "non-highway"] 5] [[:in "cost_category" "residential"] 6] [:default 6]]

        c {"cost_category" "soft"}
        ]
    (matching-rule c rules)
    (assign-matching-value c rules :foo)
    )
  )

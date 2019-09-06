(ns thermos-util
  (:require [clojure.string :as string]
            [clojure.test :as test]))

(defn assoc-by
  "Given a sequence `s` and a function `f`, returns a map from (f x) to
  x for each x in s. If there are multiple x in s with same (f x), the
  last one is what you find in the map"
  {:test
   #(do (test/is (= (assoc-by [1 2 3 4] even?) {false 3 true 4}))
        (test/is (= (assoc-by ["this" "is" "great"] first) {\t "this"
                                                           \i "is"
                                                           \g "great"})))}
  [s f]
  (reduce #(assoc %1 (f %2) %2)  {} s))

(defn distinct-by
  "Given a sequence `values` and a function `f`, returns a sequence
  containing only the first x in values for a given value of (f x)

  Order of the input will be reversed."
  {:test
   #(do (assert (= (set (distinct-by [1 2 3 4] even?)) #{1 2})))}
  
  [values f]
  (let [seen (volatile! #{})]
    (reduce
     (fn [a values]
       (let [vf (f values)]
         (if (contains? @seen vf)
           (do
             (println "Removing duplicate:" vf values)
             a)
           (do (vswap! seen conj vf)
               (cons values a)))))
     nil values)))

(defn assoc-when
  "If v is not false, assoc it to m under k, otherwise m"
  [m k v] (if v (assoc m k v) m))

(defn as-integer
  "Try and turn v into an int. Nil if we can't."
  [v]
  (cond
    (string? v)
    #?(:cljs (let [x (js/parseInt v)]
               (and x (js/isFinite x) x))
       :clj (try (Integer/parseInt v)
                 (catch NumberFormatException e)))
    (number? v) (int v)))

(defn as-double
  "Try and turn v into a double. Nil if we can't."
  [v]
  (cond
    (string? v)
    #?(:cljs (let [x (js/parseFloat v)]
               (and x (js/isFinite x) x))
       :clj (try (Double/parseDouble v)
                 (catch NumberFormatException e)))
    
    (number? v) (double v)))

(def truth-values #{"true" "TRUE" "yes" "YES" "1" 1 1.0 true})
(def false-values #{"false" "FALSE" "no" "NO" "0" "-1" 0 0.0 -1 -1.0 false})

(defn as-boolean
  "Try and turn v into a boolean.
  #{true, yes, 1} => true
  #{false, no, 0, -1} => false
  else nil"
  [v]
  (when v
    (cond
      (contains? truth-values v) true
      (contains? false-values v) false)))

(def HOURS-PER-YEAR 8766.0)
(defn annual-kwh->kw [kwh-pa]
  (/ kwh-pa HOURS-PER-YEAR))

(defn kw->annual-kwh [kwh]
  (* kwh HOURS-PER-YEAR))

(defn to-fixed [num digits]
  #?(:clj
     (.format (java.text.DecimalFormat.
               (apply str "0." (repeat digits "0")))
              num)

     :cljs
     (.toFixed num digits)))

(defn next-integer-key
  "Given a map which contains keys that are numeric, return the next
  unused key."
  [map]
  (inc (reduce max -1 (keys map))))

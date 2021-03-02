(ns thermos-util
  (:require [clojure.string :as string]
            [clojure.test :as test]))

(defn count-if [coll pred]
  (reduce (fn [a v] (if (pred v) (inc a) a)) 0 coll))

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

(defmacro for-map [[binding values & stuff] a & [b]]
  `(into
    {}
    (for [~binding ~values ~@stuff]
      ~(if b
         [a b]
         [(first binding) a]))))

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
           a
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
    #?(:cljs (if-let [x (js/parseInt v)]
               (when (js/isFinite x) x))
       :clj (try (Integer/parseInt v)
                 (catch NumberFormatException e)))
    (number? v) (int v)))

(defn parse-double [v]
  (and (string? v)
       #?(:cljs (when-let [x (js/parseFloat v)]
                  (when (js/isFinite x) x))
          :clj (try (Double/parseDouble v)
                    (catch NumberFormatException e)))))

(let [comma-number #"^\s*\d+,\d+\s*$"]
  (defn as-double
    "Try and turn v into a double. Nil if we can't."
    {:test #(do (test/is (= 1.0 (as-double "1.0")))
                (test/is (= 1.5 (as-double "1,5")))
                (test/is (nil? (as-double "1,5,")))
                (test/is (nil? (as-double nil)))
                (test/is (= 1.0 (as-double 1.0)))
                (test/is (nil? (as-double false)))
                (test/is (nil? (as-double "a1"))))}
    [v]
    (cond
      (string? v)
      (parse-double
       (if (re-matches comma-number v)
         (string/replace v \, \.)
         v))
      
      (number? v) (double v))))

(defmacro safe-div [a b]
  `(let [a# ~a]
     (if (zero? a#) 0
         (/ a# ~b))))

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

(defn format-seconds [s]
  (let [s (int s)
        seconds-part (mod s 60)
        minutes-part (int (/ s 60))
        hours-part (int (/ minutes-part 60))
        minutes-part (mod minutes-part 60)]
    (str
     (if (pos? hours-part)
       (str hours-part "h, ") "")
     (if (pos? minutes-part)
       (str minutes-part "m, ") "")
     seconds-part "s")))

(defn next-id [m]
  (inc (reduce max -1 (keys m))))

(defn assoc-id [m v]
  (assoc m (next-id m) v))

(ns thermos-util.pipes
  (:require [clojure.test :as test]))

(def density  975.0)
(def heat-capacity 4.18)
(def min-diameter 0.00)
(def max-diameter 0.5)
(def diameter-step 0.05)
(def diameter-range (range min-diameter max-diameter diameter-step))

(defn kw-per-m [^double diameter
            ^double delta-t]
  (let [area     (* Math/PI (Math/pow (* diameter 0.5) 2.0))
        velocity (- (* 4.7617 (Math/pow diameter 0.3701))
                    0.4834)
        flow-rate (* area velocity)
        mass-rate (* flow-rate density)]
    (* mass-rate heat-capacity delta-t)))

(defn linear-evaluate [curve x]
  {:test
   #(do (test/is (= (linear-evaluate [[0 0] [10 10]] 5) 5))
        (test/is (= (linear-evaluate [[0 0] [10 10]] 0) 0))
        (test/is (= (linear-evaluate [[0 0] [10 10]] 8) 8))
        (test/is (= (linear-evaluate [[0 0] [10 10]] 12) 10))
        (test/is (= (linear-evaluate [[0 0] [10 10]] -1) 0))
        (test/is (= (linear-evaluate [[0 0] [10 10] [13 20]] 13) 20))
        (test/is (= (linear-evaluate [[0 0] [10 10] [12 20]] 11) 15)))}
    
  ;; binary search

  (let [position #?(:clj (java.util.Collections/binarySearch curve [x]
                                                             #(<= (first %1)
                                                                  (first %2)))
                    :cljs 0)]
    ;; TODO cljs is broken here, need a cljs binarysearch
    (let [position (if (neg? position)
                     (- (- position) 1)
                     position)]
      (if (= position (count curve))
        (second (last curve))

        (let [[px py] (nth curve position)]
          (if (or (== x px) (zero? position))
            py
            (let [[px2 py2] (nth curve (dec position))
                  m (/ (- py py2) (- px px2))
                  fr (- x px2)]
              (+ py2 (* fr m)))))))))

(defn- la-solution [k1 k2 k3 k4 k5]
  (let [top (- (* 2 k2 k3)
               (* k4 k5))
        den (- (* k5 k5) (* 4 k1 k3))
        m (if (zero? top) 0.0 (/ top den))
        top (+ k2 (* 2 m k1))
        c (if (zero? top) 0 (/ (- top) k5))]
    [c m]))

(defn linear-approximate
  "Curve is a series of x,y points making a piecewise linear function.
  Return [c m] such that the curve is 'best' approximated by y=mx + c
  over the interval xmin - xmax

  The formula used comes from minimising analytically the square
  difference between the linear form and the curve. I did this by
  taking the derivative and solving where it's zero."
  [curve x-min x-max]

  {:test #(let [chk (fn [curve xmin xmax [ce me]]
                      (let [[ca ma] (linear-approximate curve xmin xmax)]
                        (test/is (< -0.1 (- ce ca) 0.1))
                        (test/is (< -0.1 (- me ma) 0.1))))]
            (chk [[0 5] [10 15]] 0 10 [5 1])
            (chk [[0 5] [10 15]] 5 10 [10 1]))}
    
  (cond
    (< (Math/abs (- x-min x-max)) 0.1)
    [(linear-evaluate curve (/ (+ x-min x-max) 2)) 0]

    (<= x-max (first (first curve)))
    [(second (first curve)) 0]

    (>= x-min (first (last curve)))
    [(second (last curve)) 0]

    :else
    ;; compute an optimal m and c to minimize error
    
    (let [imax (dec (count curve))]
      (loop [i  (int 0)
             k1 (double 0)
             k2 (double 0)
             k3 (double 0)
             k4 (double 0)
             k5 (double 0)]
        (if (= i imax) (la-solution k1 k2 k3 k4 k5)
            (let [[xi yi] (nth curve i)
                  [xj yj] (nth curve (inc i))]
              (cond
                (or (= i imax)
                    (> xi x-max))
                (la-solution k1 k2 k3 k4 k5)
                
                (< xj x-min)
                (recur (inc i) k1 k2 k3 k4 k5)

                :else
                (let [mi (/ (- yj yi) (- xj xi))
                      ci (- yi (* mi xi))
                      ;; clamp left
                      xi (if (< xi x-min) x-min xi)
                      yi (if (<= xi x-min) (+ ci (* mi x-min)) yi)
                      ;; clamp right
                      xj (if (> xj x-max) x-max xj)
                      yj (if (>= xj x-max) (+ ci (* mi x-max)) yj)

                      X3 (- (Math/pow xj 3) (Math/pow xi 3))
                      X2 (- (Math/pow xj 2) (Math/pow xi 2))
                      X1 (- xj xi)]

                  (recur (inc i)
                         (+ k1 (/ X3 3.0))
                         (+ k2 (* -2.0 mi (/ X3 3.0)) (- (* ci X2)))
                         (+ k3 X1)
                         (+ k4 (* (- mi) X2) (- (* 2 ci X1)))
                         (+ k5 X2))))))
        ))))

(def power-curve
  (memoize
   (fn [delta-t]
     (vec (for [x diameter-range]
            [(kw-per-m x delta-t) x])))))

(defn linear-cost-per-kw*
  "Given some pipe parameters, linearly approximate the cost/kw function"
  [delta-t min-kw max-kw cost-per-m]

  (linear-approximate
   (->> (power-curve delta-t)
        (map #(update % 1 cost-per-m))
        (vec))
   
   min-kw max-kw))

(def linear-cost-per-kw
  (memoize
   (fn [delta-t min-kw max-kw
        mechanical-fixed mechanical-var mechanical-expt
        civil-fixed civil-var civil-expt]

     (linear-cost-per-kw*
      delta-t min-kw max-kw
      (fn [dia-m]
        (+ civil-fixed mechanical-fixed
           (Math/pow (* mechanical-var dia-m) mechanical-expt)
           (Math/pow (* civil-var dia-m) civil-expt)))))))

(defn heat-loss-w-per-kwpm [delta-t diameter]
  (* delta-t
     (if (zero? diameter) 0 (+ (* 0.16807 (Math/log diameter)) 0.85684))))

(defn heat-loss-curve
  "Given flow, return and ground temperatures return
  a curve which relates kW of pipe peak capacity to W/m
  of heat losses in said pipe under average conditions."
  [flow return ground]
  (let [pc (power-curve (- flow return))
        delta-t (- (/ (+ flow return) 2) ground)]
    (for [[kw dia] pc]
      [kw (heat-loss-w-per-kwpm delta-t dia)])))


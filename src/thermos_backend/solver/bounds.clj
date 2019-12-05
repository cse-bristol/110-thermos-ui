(ns thermos-backend.solver.bounds
  "Functions for computing bounds to go into the ILP solver.
  Good quality bounds improve the quality & speed of solutions.

  The main things we want bounds on are:

  - For each arc
    - what's the largest capacity it could need
    - what's the smallest nonzero capacity it could need
    - what's the largest downstream count it could have
    - what's the smallest nonzero downstream count

  These bounds are used to:

  - Construct big M constraints that are at least a little tight
  - Get initial conditions for diversity & heat loss parameters
  "
  
  (:require [loom.graph :as graph]
            [loom.alg :as alg]
            [loom.io :as loom-io]
            [loom.attr :as attr]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clojure.set :as set]
            [loom.attr :as attr]))

;; (defn bridges
;;   "For `graph`, find all bridges. A bridge is an edge which if removed
;;   would divide the graph. This is a naive implementation."
;;   [graph]

;;   (let [count (atom 0)
;;         pre (atom {})
;;         low (atom {})
        
;;         dfs
;;         (fn dfs [u v]
;;           (let [count @count]
;;             (swap! pre assoc v count)
;;             (swap! low assoc v count))
;;           (swap! count inc)

;;           (mapcat
;;            (fn [w]
;;              (cond
;;                (not (@pre w))
;;                (let [nxt (dfs v w)]
;;                  (swap! low assoc v (min (@low v) (@low w)))
;;                  (if (= (@low w) (@pre w))
;;                    (conj nxt [v w])
;;                    nxt))

;;                (not= w u)
;;                (do (swap! low assoc v (min (@low v) (@pre w)))
;;                    nil)))
           
;;            (graph/successors graph v)))]
;;     (mapcat
;;      (fn [v]
;;        (when-not (@pre v)
;;          (dfs v v)))
;;      (graph/nodes graph))))


(defn- trivial-ub [g & {:keys [supply-vertices capacity demand-vertices demand]}]
  (let [total-supply (reduce + (map capacity supply-vertices))
        total-demand (reduce + (map demand demand-vertices))
        ;; TODO this does not account for losses however we add those
        ;; on in the model later but not entirely correctly. Probably
        ;; OK for such a loose UB.
        max-flow (min total-supply total-demand)]
    (into {}
          (concat (for [e (graph/edges g)] [e max-flow])
                  (for [e (graph/edges g)] [[(second e) (first e)] max-flow])))))

;; (defn- max-flow-flow-ub
;;   "
;;   A flow upper bound calculation which uses max-flow with unbounded inflow
;;   to compute a set of flow upper bounds for every arc in g.
;;   "
;;   [g & {:keys [supply-vertices demand-vertices capacity demand]}]

;;   (let [dg (graph/weighted-digraph)
;;         dg (reduce
;;             (fn [g [i j]]
;;               (graph/add-edges g [i j Double/MAX_VALUE] [j i Double/MAX_VALUE]))
;;             dg (graph/edges g))

;;         ;; add in limiter edges for demand
;;         dg (reduce
;;             (fn [g v]
;;               (graph/add-edges g [v :sink (demand v)]))
;;             dg demand-vertices)

;;         ;; add in limited edges for supply - we do not use supply
;;         ;; capacity here because we want to allow too much flow so the
;;         ;; sink edges & their connected edges get maximized.
;;         dg (reduce
;;             (fn [g v]
;;               (graph/add-edges g [:source v Double/MAX_VALUE]))
;;             dg supply-vertices)

;;         max-supply (reduce + (map capacity supply-vertices))
        
;;         [flow val] (alg/max-flow dg :source :sink)
;;         ]
;;     (->> (graph/edges g)
;;          (mapcat
;;           (fn [e]
;;             (let [e' [(second e) (first e)]
;;                   f  (min max-supply
;;                            (max (get-in flow e 0)
;;                                 (get-in flow e' 0)))]
;;               [[e f] [e' f]])))
;;          ;; if max-flow says the UB is zero, bad times for everyone
;;          ;; alternatively we could say if f is zero, it's max-supply
;;          (remove (comp zero? second))
;;          (into {}))))


;; (defn- bridge-flow-ub
;;   "A flow upper bound calculation which finds all the bridges in the
;;   graph and gives their max flow in each direction. This should
;;   actually be close to tight."
;;   [g & {:keys [demand capacity]}]

;;   (->> (for [[i j] (bridges g)]
;;          (let [g' (graph/remove-edges g [i j] [j i])
;;                i* (set (alg/bf-traverse g' i))
;;                j* (set (alg/bf-traverse g' j))

;;                di (reduce + (remove nil? (map demand i*)))
;;                ci (reduce + (remove nil? (map capacity i*)))
;;                dj (reduce + (remove nil? (map demand j*)))
;;                cj (reduce + (remove nil? (map capacity j*)))
;;                ]
;;            [[[i j] (min ci dj)]
;;             [[j i] (min cj di)]]))
;;        (apply concat)
;;        (into {})))

(defn- all-cuts-flow-ub
  "Find flow upper bounds by taking each edge, removing it, and counting up what's on either side."
  [g & {:keys [capacity demand supply-vertices]}]
  (let [supply-vertices (set supply-vertices)]
    
    (->>
     (for [[i j] (graph/edges g)]
       (let [g' (graph/remove-edges g [i j] [j i])
             i* (set (alg/bf-traverse g' i))
             j* (set (alg/bf-traverse g' j))

             di (reduce + (remove nil? (map demand i*)))
             ci (reduce + (remove nil? (map capacity i*)))
             dj (reduce + (remove nil? (map demand j*)))
             cj (reduce + (remove nil? (map capacity j*)))
             ]
         [[[i j] (min ci dj)]
          [[j i] (min cj di)]]
         ))
     (apply concat)
     (into {}))))

(defn flow-upper-bounds [g & o]
  (log/info "Finding upper bounds...")
  (let [trivial (apply trivial-ub g o)
        by-cutting (apply all-cuts-flow-ub g o)
        all (merge trivial by-cutting)]
    all
    ))

(defn edge-bounds [g & {:keys [capacity demand peak-demand size max-kwp edge-max]
                        :or {edge-max (constantly 100000000)}}]
  (->>
   (pmap (fn [[i j]]
           (let [max-kwp (edge-max i j)

                 posmin (fn [c]
                          (let [c (filter pos? c)]
                            (if (empty? c) 0
                                (reduce min c))))

                 bound (fn [a b] [(min a b max-kwp) (min b max-kwp)])
                 
                 g' (graph/remove-edges g [i j] [j i])
                 i* (set (alg/bf-traverse g' i))
                 j* (set (alg/bf-traverse g' j))

                 peak-i* (map peak-demand i*)
                 mean-i* (map demand i*)
                 cap-i* (map capacity i*)
                 count-i* (map size i*)

                 peak-j* (map peak-demand j*)
                 mean-j* (map demand j*)
                 cap-j* (map capacity j*)
                 count-j* (map size j*)

                 sum-peak-i* (reduce + peak-i*)
                 sum-mean-i* (reduce + mean-i*)
                 sum-cap-i* (reduce + cap-i*)
                 sum-count-i* (reduce + count-i*)

                 sum-peak-j* (reduce + peak-j*)
                 sum-mean-j* (reduce + mean-j*)
                 sum-cap-j* (reduce + cap-j*)
                 sum-count-j* (reduce + count-j*)

                 min-peak-i* (posmin peak-i*)
                 min-mean-i* (posmin mean-i*)
                 min-count-i* (posmin count-i*)

                 min-peak-j* (posmin peak-j*)
                 min-mean-j* (posmin mean-j*)
                 min-count-j* (posmin count-j*)]
             
             [[i j]
              {:count [[min-count-j* sum-count-j*]  ;; going forwards, we range from the smallest thing on j-side to the total on j-side
                       [min-count-i* sum-count-i*]] ;; going backwards it's the inverse
               :peak  [(bound min-peak-j* (min sum-cap-i* sum-peak-j*)) ;; going forwards, it's the smallest demand on the j side up to either flow from i or flow into j
                       (bound min-peak-i* (min sum-cap-j* sum-peak-i*))]
               :mean  [(bound min-mean-j* (min sum-cap-i* sum-mean-j*))
                       (bound min-mean-i* (min sum-cap-j* sum-mean-i*))]
               }
              ]
             )
           )
         (graph/edges g))
   (into {})))

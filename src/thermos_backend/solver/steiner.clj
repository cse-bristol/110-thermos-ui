(ns thermos-backend.solver.steiner
  (:require [thermos-backend.solver.interop :as interop]
            [thermos-specs.document :as doc]
            [thermos-util.pipes :as pipes]
            [loom.graph :as graph]
            [loom.alg :as alg]
            [loom.attr :as attr]
            [thermos-frontend.operations :refer [select-candidates]]
            [clojure.tools.logging :as log]))

(def ^:private thread-pool (java.util.concurrent.Executors/newFixedThreadPool 3))

(defn upper-triangle [coll]
  (when-let [s (next coll)]
    (lazy-cat (for [y s] [(first coll) y])
              (upper-triangle s))))

(defn tree [problem]
  (let [pipe-curves (pipes/curves problem)

        cost (memoize (fn [civil-id] (pipes/solved-principal pipe-curves civil-id 100.0)))

        graph   (interop/simplified-graph problem)
        termini (filter #(attr/attr graph % :real-vertex) (graph/nodes graph))
        
        _ (log/info "Constructing weighted graph" )
        
        wg    (reduce
               (fn add-edge [g [i j]]
                 (let [lbc (attr/attr graph [i j] :civil-costs)
                       c (reduce + (for [[id len] lbc] (* len (cost id))))]
                   
                   (cond-> g
                     (not= i j)
                     (doto
                       (.addVertex i)
                       (.addVertex j)
                       ;; don't really need anything to go in the slot?
                       (.addEdge i j [i j])
                       (.setEdgeWeight i j c)))))
               (reduce
                (fn add-vertex [g i] (doto g (.addVertex i)))
                (org.jgrapht.graph.SimpleWeightedGraph. Object)
                (graph/nodes graph))
               (graph/edges graph))

        _ (log/info "Finding shortest paths")

        spa (org.jgrapht.alg.shortestpath.CHManyToManyShortestPaths.
             wg thread-pool)

        mmsp (.getManyToManyPaths spa (set termini) (set termini))

        _ (log/info "Making pairwise terminal graph")
        
        mmgraph (reduce
                 (fn [g [i j]]
                   (let [path (.getPath mmsp i j)]
                     (cond-> g
                       path
                       (doto 
                         (.addVertex i)
                         (.addVertex j)
                         (.addEdge i j path)
                         (.setEdgeWeight i j (.getWeight path))))))
                 (org.jgrapht.graph.SimpleWeightedGraph. org.jgrapht.GraphPath)
                 (upper-triangle termini))
        
        _ (log/info "Find MST on pairwise terminal graph")
        msta (org.jgrapht.alg.spanning.KruskalMinimumSpanningTree.
              mmgraph)

        mst (.getSpanningTree msta)
        ]
    (reduce
     (fn [problem edge]
       (let [ids (attr/attr graph edge :ids)]
         (select-candidates problem ids :union)))
     (select-candidates problem #{} :replace)
     (for [path mst edge (.getEdgeList path)] edge))))


(ns thermos-backend.solver.steiner
  (:require [thermos-backend.solver.interop :as interop]
            [thermos-specs.document :as doc]
            [thermos-util.pipes :as pipes]
            [loom.graph :as graph]
            [loom.alg :as alg]
            [loom.attr :as attr]
            [thermos-frontend.operations :refer [select-candidates
                                                 selected-candidates
                                                 subset-document-to-selected-candidates]]
            [clojure.tools.logging :as log]
            [thermos-backend.solver.logcap :as logcap]
            [thermos-specs.candidate :as candidate]
            [thermos-specs.document :as document]))

(def ^:private thread-pool (java.util.concurrent.Executors/newFixedThreadPool 3))

(defn upper-triangle [coll]
  (when-let [s (next coll)]
    (lazy-cat (for [y s] [(first coll) y])
              (upper-triangle s))))

(defn tree- [problem]
  (let [pipe-curves (pipes/curves problem)

        cost (memoize (fn [civil-id] (pipes/solved-principal pipe-curves civil-id 100.0)))

        problem (cond-> problem
                  (seq (selected-candidates problem))
                  (subset-document-to-selected-candidates))
        
        graph   (interop/simplified-graph problem)
        termini (set (filter #(attr/attr graph % :real-vertex) (graph/nodes graph)))

        candidates (::document/candidates problem)
        
        _ (log/info "Constructing weighted graph" )
        
        wg    (reduce
               (fn add-edge [g [i j]]
                 (let [lbc (attr/attr graph [i j] :civil-costs)
                       c (reduce + (for [[id len] lbc] (* len (cost id))))
                       existing-terminal-edge (or (and (contains? termini i)
                                                       (contains? (.vertexSet g) i)
                                                       (pos? (.outDegreeOf g i))
                                                       (not (candidate/has-supply? (candidates i))))
                                                  
                                                  (and (contains? termini j)
                                                       (contains? (.vertexSet g) j)
                                                       (pos? (.outDegreeOf g j))
                                                       (not (candidate/has-supply? (candidates j)))))
                       ]
                   (when existing-terminal-edge
                     (log/warnf "Edge %s->%s might make the steiner tree bad - skipping it" i j))
                   
                   (cond-> g
                     (and (not existing-terminal-edge)
                          (not= i j))
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


(defn tree [problem progress]  
  (let [log-writer (java.io.StringWriter.)]
    (try
      (progress :message "Find Steiner tree")
      (logcap/with-log-messages
        (fn [^String msg]
          (.write log-writer msg)
          (.append log-writer \newline)
          (progress :message (.toString log-writer)))
        (tree- problem))
      (catch InterruptedException ex (throw ex))
      (catch Throwable ex
        (progress :message
                  (with-out-str
                    (println (.toString log-writer))
                    (println "---")
                    (clojure.stacktrace/print-throwable ex)
                    (println "---")
                    (clojure.stacktrace/print-cause-trace ex)))
        problem))))


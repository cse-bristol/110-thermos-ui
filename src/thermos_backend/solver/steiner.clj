(ns thermos-backend.solver.steiner
  (:require [thermos-backend.solver.interop :as interop]
            [thermos.opt.net.pipes :as pipes]
            [thermos.opt.net.heuristic.steiner :as steiner]
            [thermos.opt.net.heuristic.refine :as refine]
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

(defn tree-
  "Select an approximate Steiner tree joining up the buildings in
  `problem` (or the selected part of it), using the shortest path
  heuristic from thermos.opt.net.heuristic.steiner.

  The tree grows out from the supplies; a connected part of the
  network with no supply grows from one of its buildings instead."
  [problem]
  (let [pipe-curves (interop/doc->pipe-curves problem)

        cost (memoize (fn [civil-id] (pipes/solved-principal pipe-curves civil-id 100.0)))

        problem (cond-> problem
                  (seq (selected-candidates problem))
                  (subset-document-to-selected-candidates))

        graph   (interop/simplified-graph problem)
        candidates (::document/candidates problem)

        nodes   (vec (graph/nodes graph))
        index   (into {} (map-indexed (fn [i v] [v i])) nodes)
        termini (set (filter #(attr/attr graph % :real-vertex) nodes))

        owners  (let [supplies (set (filter #(candidate/has-supply? (candidates %)) termini))]
                  (into supplies
                        (for [component (alg/connected-components graph)
                              :let [component-termini (filter termini component)]
                              :when (and (seq component-termini)
                                         (not-any? supplies component-termini))]
                          (first component-termini))))

        real-edges (->> (graph/edges graph)
                        (map (comp vec sort))
                        (remove (fn [[i j]] (= i j)))
                        (distinct)
                        (vec))
        n-real     (count real-edges)
        root       (count nodes)

        _ (log/info "Constructing weighted graph:"
                    (count nodes) "vertices,"
                    n-real "edges,"
                    (count termini) "buildings,"
                    (count owners) "roots")

        us (int-array (concat (map (comp index first) real-edges)
                              (repeat (count owners) root)))
        vs (int-array (concat (map (comp index second) real-edges)
                              (map index owners)))
        ws (double-array (concat (for [e real-edges]
                                   (reduce + (for [[id len] (attr/attr graph e :civil-costs)]
                                               (* len (cost id)))))
                                 (repeat (count owners) 0.0)))

        owner?    (let [a (boolean-array (inc root))]
                    (doseq [o owners] (aset a (int (index o)) true))
                    a)
        terminal? (let [a (boolean-array (inc root))]
                    (doseq [t termini] (aset a (int (index t)) true))
                    a)

        steiner-graph
        {:n (inc root) :us us :vs vs :ws ws
         :adj (steiner/adjacency (inc root) us vs)
         ;; buildings are only at the ends of paths; a root building is
         ;; only passed through on the way out from the virtual root
         :open? (fn [v pred-edge]
                  (cond
                    (== v root)      true
                    (aget owner? v)  (>= pred-edge n-real)
                    (aget terminal? v) false
                    :else true))
         :demand? (fn [v] (and (< v root) (aget terminal? v) (not (aget owner? v))))}

        terminals (map index termini)

        _ (log/info "Finding shortest path tree")
        tree (steiner/shortest-path-tree steiner-graph root terminals)

        {:keys [tree before after moves]}
        (refine/improve-tree steiner-graph root terminals tree)

        _ (log/infof "Refined tree: %d moves, cost %.0f -> %.0f" moves before after)]

    (reduce
     (fn [problem edge]
       (select-candidates problem (attr/attr graph edge :ids) :union))
     (select-candidates problem #{} :replace)
     (for [e tree :when (< e n-real)] (real-edges e)))))


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

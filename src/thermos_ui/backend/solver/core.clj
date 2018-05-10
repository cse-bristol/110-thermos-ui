(ns thermos-ui.backend.solver.core
  (:require [thermos-ui.backend.solver.interop :as interop]
            [thermos-ui.backend.problems.db :as problem-db]
            [clojure.edn :as edn]
            [thermos-ui.backend.queue :as queue]))

(defn consume-problem [conn args]
  ;; TODO I am reading off another connection here
  ;; rather than within my own transaction
  
  (let [problem-data (problem-db/get-content (:org args) (:name args) (:id args))
        problem-data (edn/read-string problem-data)
        solved-problem (interop/solve problem-data)
        ]
    ;; we want to replace the problem content with the version that
    ;; has a solution, and we want to update the table to say we have
    ;; a solution as well. I think probably this needs connecting up
    ;; using something like Component in the end.
    (problem-db/add-solution (:org args) (:name args) (:id args)
                             (pr-str solved-problem))))


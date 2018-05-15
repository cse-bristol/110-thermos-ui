(ns thermos-ui.backend.solver.core
  (:require [thermos-ui.backend.solver.interop :as interop]
            [thermos-ui.backend.problems.db :as problem-db]
            [clojure.edn :as edn]))

(defn consume-problem [conn args]
  ;; TODO I am reading off another connection here
  ;; rather than within my own transaction
  
  (let [problem-data (problem-db/get-content (:org args) (:name args) (:id args))
        problem-data (edn/read-string problem-data)
        solved-problem (interop/solve problem-data)
        ]
    ;; this is a bit ugly - once the problem is solved, we update the
    ;; database outside our transaction, which is all wrong.
    (problem-db/add-solution (:org args) (:name args) (:id args)
                             (pr-str solved-problem))))



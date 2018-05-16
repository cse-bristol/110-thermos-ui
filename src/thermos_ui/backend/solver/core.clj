(ns thermos-ui.backend.solver.core
  (:require [thermos-ui.backend.solver.interop :as interop]
            [thermos-ui.backend.problems.db :as problem-db]
            [clojure.edn :as edn]
            [thermos-ui.backend.queue :as queue]
            [com.stuartsierra.component :as component]
            
            ))

(defn consume-problem [config conn args]
  ;; TODO I am reading off another connection here
  ;; rather than within my own transaction
  
  (let [problem-data (problem-db/get-content conn (:org args) (:name args) (:id args))
        problem-data (edn/read-string problem-data)
        solved-problem (interop/solve config problem-data)
        ]
    ;; this is a bit ugly - once the problem is solved, we update the
    ;; database outside our transaction, which is all wrong.
    (problem-db/add-solution conn
                             (:org args) (:name args) (:id args)
                             (pr-str solved-problem))))

(defrecord Solver [config queue database]
  component/Lifecycle
  (start [component]
    (queue/consume queue :problems (partial consume-problem (:config component)))
    component)
  
  (stop [component]))

(defn new-solver [config] (map->Solver {:config config}))

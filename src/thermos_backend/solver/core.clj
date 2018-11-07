(ns thermos-backend.solver.core
  (:require [thermos-backend.solver.interop :as interop]
            [thermos-backend.problems.db :as problem-db]
            [clojure.edn :as edn]
            [thermos-backend.queue :as queue]
            [com.stuartsierra.component :as component]
            
            ))

;; TODO change the storage format to prevent inclusion of unknown tags?

(defrecord TaggedValue [tag value])

(defmethod print-method TaggedValue [this ^java.io.Writer w]
   (.write w "#")
   (print-method (:tag this) w)
   (.write w " ")
   (print-method (:value this) w))

(defn consume-problem [config conn [org name id]]
  ;; TODO I am reading off another connection here
  ;; rather than within my own transaction
  
  (let [problem-data (problem-db/get-content conn org name id)
        problem-data (edn/read-string {:default ->TaggedValue} problem-data)
        solved-problem (interop/solve (format "%s-%s-%s-" org name id)
                                      config problem-data)
        ]
    (problem-db/add-solution conn
                             org name id
                             (pr-str solved-problem))))

(defrecord Solver [config queue database]
  component/Lifecycle
  (start [component]
    (println "Reading problems from queue...")
    (let [config (:config component)]
      (queue/consume queue :problems
                     (fn [connection args]
                       (consume-problem
                        config
                        connection
                        args))))
    component)
  
  (stop [component]))

(defn new-solver [config] (map->Solver {:config config}))

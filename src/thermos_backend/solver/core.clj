(ns thermos-backend.solver.core
  (:require [thermos-backend.solver.interop :as interop]
            [thermos-backend.db.projects :as projects]
            [clojure.edn :as edn]
            [thermos-backend.queue :as queue]
            [mount.core :refer [defstate]]))

;; TODO change the storage format to prevent inclusion of unknown tags?

(defrecord TaggedValue [tag value])

(defmethod print-method TaggedValue [this ^java.io.Writer w]
   (.write w "#")
   (print-method (:tag this) w)
   (.write w " ")
   (print-method (:value this) w))

(defn queue-problem [network-id]
  (let [job-id (queue/enqueue :problems network-id)]
    (projects/associate-job! network-id job-id)))

(defn consume-problem [network-id]
  (let [{problem-data :content}
        (projects/get-network network-id :include-content true)
        
        problem-data (edn/read-string {:default ->TaggedValue} problem-data)
        solved-problem (interop/solve (format "network-%s-" network-id) problem-data)]
    (projects/add-solution! network-id (pr-str solved-problem))))

(defstate queue-consumer
  :start
  (queue/consume :problems consume-problem))

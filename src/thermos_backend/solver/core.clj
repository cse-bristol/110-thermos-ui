(ns thermos-backend.solver.core
  (:require [thermos-backend.solver.interop :as interop]
            [thermos-backend.solver.supply :as supply-solver]
            [thermos-backend.db.projects :as projects]
            [thermos-backend.config :refer [config]]
            [clojure.edn :as edn]
            [thermos-backend.queue :as queue]
            [mount.core :refer [defstate]]
            [clojure.tools.logging :as log]
            [thermos-specs.document :as document]))

;; TODO change the storage format to prevent inclusion of unknown tags?

(defrecord TaggedValue [tag value])

(defmethod print-method TaggedValue [this ^java.io.Writer w]
   (.write w "#")
   (print-method (:tag this) w)
   (.write w " ")
   (print-method (:value this) w))

(defn queue-problem [network-id problem-type]
  {:pre [(#{:network :supply :both} problem-type)]}

  (let [job-id (queue/enqueue :problems
                              {:id network-id
                               :solve problem-type})]
    (projects/associate-job! network-id job-id)))

(defn consume-problem [{network-id :id problem-type :solve}
                       progress]
  {:pre [(int? network-id) (#{:network :supply :both} problem-type)]}
  (try
    (-> (projects/get-network network-id :include-content true)
        (:content)
        (->> (edn/read-string {:default ->TaggedValue}))

        (cond->
            (#{:network :both} problem-type)
          (->> (interop/try-solve (format "network-%s-" network-id)))
          
          (#{:supply :both} problem-type)
          (->> (supply-solver/try-solve (format "supply-%s-" network-id))))
        
        (pr-str)
        (->> (projects/add-solution! network-id)))
    (catch Exception ex
      (log/error "Error caught in problem consumer" ex))))


(defstate queue-consumer
  :start
  (queue/consume :problems
                 (config :solver-count 4)
                 consume-problem))

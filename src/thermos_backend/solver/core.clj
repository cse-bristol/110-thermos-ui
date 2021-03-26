(ns thermos-backend.solver.core
  (:require [thermos-backend.solver.interop :as interop]
            [thermos-backend.solver.supply :as supply-solver]
            [thermos-backend.db.projects :as projects]
            [thermos-backend.config :refer [config]]
            [clojure.edn :as edn]
            [thermos-backend.queue :as queue]
            [mount.core :refer [defstate]]
            [clojure.tools.logging :as log]
            [thermos-specs.document :as document]
            [thermos-specs.supply :as supply]))

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

(defn- with-restricted-runtime
  "Restrict the maximum runtime for problems that are part of restricted projects."
  [document map-id]
  (let [auth (projects/most-permissive-map-user-auth map-id)
        max-project-runtime (auth (config :max-project-runtime))]
    (if max-project-runtime

      (let [max-runtime
            (min (::document/maximum-runtime document)
                 max-project-runtime)

            max-supply-runtime
            (min (get-in [::supply/objective :time-limit] document)
                 max-project-runtime)]

        (-> document
            (assoc ::document/maximum-runtime max-runtime)
            (assoc-in [::supply/objective :time-limit] max-supply-runtime)))

      document)))

(defn consume-problem [{network-id :id problem-type :solve}
                       progress]
  {:pre [(int? network-id) (#{:network :supply :both} problem-type)]}
  (try
    (let [network (projects/get-network network-id :include-content true)

          problem (with-restricted-runtime
                    (edn/read-string {:default ->TaggedValue} (:content network))
                    (:map-id network))

          solution (cond-> problem
                     (#{:network :both} problem-type)
                     (interop/try-solve progress)

                     (#{:supply :both} problem-type)
                     (supply-solver/try-solve))]

      (projects/add-solution! network-id (pr-str solution)))

    (catch InterruptedException ex
      (log/info "Solver interrupted (probably user cancel) for" network-id)
      (projects/forget-run! network-id))
    (catch Exception ex
      (log/error "Error caught in problem consumer" ex))))


(defstate queue-consumer
  :start
  (queue/consume :problems
                 (config :solver-count 4)
                 consume-problem))

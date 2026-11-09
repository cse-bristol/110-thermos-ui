;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.solver.core
  (:require [thermos-backend.solver.interop :as interop]
            [thermos-backend.solver.steiner :as steiner]
            [thermos-backend.solver.supply :as supply-solver]
            [thermos-backend.db.projects :as projects]
            [thermos-backend.config :refer [config]]
            [clojure.edn :as edn]
            [thermos-backend.queue :as queue]
            [mount.core :refer [defstate]]
            [clojure.tools.logging :as log]
            [thermos-specs.document :as document]
            [thermos-specs.solution :as solution]
            [thermos-specs.supply :as supply]
            [thermos-specs.view :as view]))

;; TODO change the storage format to prevent inclusion of unknown tags?

(defrecord TaggedValue [tag value])

(defmethod print-method TaggedValue [this ^java.io.Writer w]
   (.write w "#")
   (print-method (:tag this) w)
   (.write w " ")
   (print-method (:value this) w))

(defn- with-restricted-runtime
  "Restrict the maximum runtime for problems that are part of restricted projects."
  [document map-id]
  (let [auth (projects/most-permissive-map-user-auth map-id)
        max-project-runtime (auth (config :max-project-runtime))]
    (if max-project-runtime

      (let [max-runtime
            (min (or (::document/maximum-runtime document) 1.0)
                 max-project-runtime)

            max-supply-runtime
            (min (or (get-in document [::supply/objective :time-limit]) 1.0)
                 max-project-runtime)]

        (-> document
            (assoc ::document/maximum-runtime max-runtime)
            (assoc-in [::supply/objective :time-limit] max-supply-runtime)))

      document)))

(defn- expected-runtime
  "How many seconds a job of `problem-type` on `document` could take,
  given its time limits"
  [document problem-type]
  (let [network (* 3600.0 (or (::document/maximum-runtime document) 1.0))
        supply  (* 3600.0 (or (get-in document [::supply/objective :time-limit]) 1.0))]
    (case problem-type
      :network network
      :supply  supply
      :both    (+ network supply)
      ;; tree and tidy are quick
      60.0)))

(defn queue-problem [network-id problem-type]
  {:pre [(#{:network :supply :both
            :tidy :tree}
          problem-type)]}
  (let [network (projects/get-network network-id :include-content true)
        document (with-restricted-runtime
                   (edn/read-string {:default ->TaggedValue} (:content network))
                   (:map-id network))
        job-id (queue/enqueue :problems
                              {:id network-id
                               :solve problem-type}
                              :expected-runtime (expected-runtime document problem-type))]
    (projects/associate-job! network-id job-id)))

(defn- show-solution
  "Switch `solution` to the editor tab which best shows how a
  `problem-type` run went: the run log if the network model crashed,
  the summary if it found no solution, the supply solution after a
  supply run on its own or a supply crash (that page shows its log),
  and otherwise the map, unless already on a solution tab."
  [solution problem-type]
  (let [tab (get-in solution [::view/view-state ::view/selected-tab])
        network-run (#{:network :both} problem-type)]
    (cond
      (and network-run (= :uncaught-error (::solution/state solution)))
      (view/switch-to-tab solution :run-log)

      (and network-run (not (solution/exists? solution)))
      (view/switch-to-tab solution :solution)

      (or (= :supply problem-type)
          (= :uncaught-error
             (get-in solution [::solution/supply-solution :state])))
      (view/switch-to-tab solution :supply-solution)

      ;; tree and tidy don't solve anything
      (not network-run)
      solution

      (#{:solution :supply-solution} tab)
      solution

      :else
      (view/switch-to-map solution))))

(defn consume-problem [{network-id :id problem-type :solve}
                       progress]
  {:pre [(int? network-id) (#{:network :supply :both :tree :tidy} problem-type)]}
  (try
    (let [network (projects/get-network network-id :include-content true)

          problem (with-restricted-runtime
                    (edn/read-string {:default ->TaggedValue} (:content network))
                    (:map-id network))

          solution (cond-> problem
                     (= :tree problem-type)
                     (steiner/tree progress)

                     (= :tidy problem-type)
                     (interop/tidy progress)

                     (#{:network :both} problem-type)
                     (interop/try-solve progress))

          ;; Stopping (queue/cancel) interrupts this thread. The network
          ;; model then stops early and keeps its best solution, so once
          ;; it has one a stop means to keep that, without the supply.
          solution (cond
                     (= :supply problem-type)
                     (supply-solver/try-solve solution)

                     (not= :both problem-type)
                     solution

                     (or (::solution/stopped-early solution)
                         (Thread/interrupted))
                     solution

                     :else
                     (try (supply-solver/try-solve solution)
                          (catch InterruptedException _ solution)))]

      ;; a shutdown interrupts this too, but the job will be run again
      (when (queue/shutting-down?)
        (throw (InterruptedException. "Shutting down")))
      ;; a stop which came too late to make any difference
      (Thread/interrupted)
      (projects/add-solution! network-id
                              (pr-str (show-solution solution problem-type))))

    (catch InterruptedException ex
      (log/info "Solver interrupted (probably user cancel) for" network-id)
      ;; on shutdown the job stays the network's, to be run again
      (when-not (queue/shutting-down?)
        (projects/forget-run! network-id))
      :cancelled)
    (catch Exception ex
      (if (:cancelled (ex-data ex))
        (do (log/info "Solver cancelled for" network-id)
            (projects/forget-run! network-id)
            :cancelled)
        (log/error "Error caught in problem consumer" ex)))))


(defstate queue-consumer
  :start
  (queue/consume :problems
                 (config :solver-count 4)
                 consume-problem))

;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.queue
  (:require [thermos-backend.db :as db]
            [thermos-backend.config :refer [config]]
            [honeysql.format :refer [format-lock-clause]]
            [honeysql.core :as sql]
            [honeysql-postgres.helpers :refer :all]
            [honeysql-postgres.format]
            [honeysql.helpers :refer :all]
            [jdbc.core :as jdbc]
            [clojure.tools.logging :as log]
            [clojure.edn :as edn]
            [clojure.set :refer [map-invert]]
            [mount.core :refer [defstate]]))


(def READY_STATE (sql/call :job_state "ready"))
(def RUNNING_STATE (sql/call :job_state "running"))
(def COMPLETE_STATE (sql/call :job_state "completed"))
(def FAILED_STATE (sql/call :job_state "failed"))
(def CANCEL_REQUESTED_STATE (sql/call :job_state "cancel"))
(def CANCELLING_STATE (sql/call :job_state "cancelling"))
(def CANCELLED_STATE (sql/call :job_state "cancelled"))
(def CANCELABLE_STATES #{READY_STATE RUNNING_STATE})

(def BLOCKING_STATES #{READY_STATE
                       RUNNING_STATE
                       CANCEL_REQUESTED_STATE
                       CANCELLING_STATE})

(def FINISHED_STATES #{COMPLETE_STATE FAILED_STATE CANCELLED_STATE})

(defn finished?
  "Is the given task, such as might be returned by `list-tasks`, in a finished state"
  [task]
  (#{:complete :failed :cancelled} (keyword (:state task))))

(defmethod format-lock-clause :skip-locked [_] "FOR UPDATE SKIP LOCKED")

(def consumers
  "Maps from queue names to callback + capacity"
  (atom {}))

(def running-jobs (atom {})) ;; {queue {job-id thread}}

(defonce ^:private shutting-down
  ;; true while poll-thread is stopping, when running jobs are interrupted
  ;; to be put back on the queue, rather than to stop them
  (atom false))

(defn shutting-down?
  "Whether the queue is stopping. A job which is interrupted because of
  this will be run again, so should not keep anything it has done."
  []
  @shutting-down)

(defonce finish-listeners
  (atom {})) ;; {queue {key fn}}

(defn on-finish
  "Call `f` when a job on `queue-name` finishes, with a map having
  :job-id, :args, :state (:completed :failed or :cancelled) and :message.
  It is called on the job's thread, so should be quick. `key` identifies
  the listener so it can be replaced."
  [queue-name key f]
  (swap! finish-listeners assoc-in [queue-name key] f))

(defn- notify-finished [queue-name job]
  (doseq [[key f] (get @finish-listeners queue-name)]
    (try (f job)
         (catch Exception e
           (log/error e "Job finish listener" key "failed for" queue-name job)))))

(defn- get-spare-slots
  "
  Returns a map which says for each queue name how many jobs we could run right now
  Suffers from a race where if a job starts while this is evaluating, we will misreport.

  However, a robust guarantee that we do not run too many jobs is not important.
  If this proves a problem later we can use some atomic to lock for capacity, and
  return the job to the queue if we might be wrong.
  "
  []
  (into {}
        (let [running-jobs @running-jobs
              consumers @consumers]
          (for [[queue-name {capacity :capacity}] consumers]
            [queue-name (max 0 (- capacity (count (running-jobs queue-name))))]))))

(defn- queue-priority [auth]
  (let [weight (auth (config :priority-queue-weight))]
    (if (nil? weight) 0 weight)))

(defn get-queue-query [queue-name n]
  (case queue-name
    :problems 
    (->
     (select :*)
     (from
      [(-> (select :jobs.id :queue-name :args)
           (from :jobs)
           (join :networks [:= :networks.job-id :jobs.id]
                 :users [:= :networks.user-id :users.id])
           (where [:and
                   [:= :state READY_STATE]
                   [:= :queue-name (name queue-name)]])
           (lock :mode :skip-locked)
           (order-by [(sql/call :case
                                [:= :users.auth (sql/call :user_auth (name :basic))] (queue-priority :basic)
                                [:= :users.auth (sql/call :user_auth (name :intermediate))] (queue-priority :intermediate)
                                [:= :users.auth (sql/call :user_auth (name :unlimited))] (queue-priority :unlimited)
                                [:= :users.auth (sql/call :user_auth (name :admin))] (queue-priority :admin)
                                :else 0) :asc]
                     [:jobs.queued :asc])
           (limit n))
       :stuff]))
    (->
     (select :*)
     (from
      [(-> (select :id :queue-name :args)
           (from :jobs)
           (where [:and
                   [:= :state READY_STATE]
                   [:= :queue-name (name queue-name)]])
           (lock :mode :skip-locked)
           (limit n))
       :stuff]))))

(def ^:private default-expected-runtime
  "Seconds to assume a job will take, if it was queued without saying"
  3600.0)

(defn estimate-timing
  "Estimate when job `job-id` will start and finish, given `capacity`
  slots and `jobs`, the waiting and running jobs on its queue, as maps
  with :id, :state, :expected-runtime, and in seconds :elapsed since
  they started and :waited since they were queued, and :priority.

  Every job is supposed to run for its whole expected runtime, so these
  are more like upper bounds than estimates. Running jobs hold their
  slot until they run out of time, and queued jobs are taken in order
  of :priority and then the longest waiting, as `get-queue-query`
  takes them.

  Returns nil if the job is not in `jobs`, or else a map of seconds
  {:elapsed :expected-runtime :starts-in :finishes-in}, and for a
  queued job its :position in the queue, counting from 1."
  [capacity jobs job-id]
  (let [runtime  #(double (or (:expected-runtime %) default-expected-runtime))
        elapsed  #(double (or (:elapsed %) 0.0))
        running? #(not= "ready" (str (:state %)))
        {running true queued false} (group-by running? jobs)
        this-job (first (filter #(= job-id (:id %)) jobs))]
    (when this-job
      (if (running? this-job)
        {:elapsed (elapsed this-job)
         :expected-runtime (runtime this-job)
         :starts-in 0.0
         :finishes-in (max 0.0 (- (runtime this-job) (elapsed this-job)))}

        ;; each slot is free from some number of seconds from now
        (loop [position 1
               slots (sort (take (max 1 capacity)
                                 (concat (sort (for [j running]
                                                 (max 0.0 (- (runtime j) (elapsed j)))))
                                         (repeat 0.0))))
               [job & more] (sort-by (juxt #(:priority % 0)
                                           #(- (double (or (:waited %) 0.0))))
                                     queued)]
          (let [start  (first slots)
                finish (+ start (runtime job))]
            (if (= job-id (:id job))
              {:elapsed 0.0
               :expected-runtime (runtime job)
               :starts-in start
               :finishes-in finish
               :position position}
              (recur (inc position) (sort (cons finish (rest slots))) more))))))))

(defn job-timing
  "`estimate-timing` for job `job-id` on the problems queue"
  [job-id]
  {:pre [(int? job-id)]}
  (estimate-timing
   (get-in @consumers [:problems :capacity] (config :solver-count))
   (for [job (-> (select :jobs.id :jobs.state :jobs.expected-runtime :users.auth
                         [(sql/raw "extract(epoch from (now() at time zone 'utc') - jobs.started)")
                          :elapsed]
                         [(sql/raw "extract(epoch from (now() at time zone 'utc') - jobs.queued)")
                          :waited])
                 (from :jobs)
                 (left-join :networks [:= :networks.job-id :jobs.id]
                            :users [:= :networks.user-id :users.id])
                 (where [:and
                         [:= :queue-name "problems"]
                         [:in :state BLOCKING_STATES]])
                 (db/fetch!))]
     (assoc job :priority (queue-priority (keyword (str (or (:auth job) "basic"))))))
   job-id))

(defn claim-jobs
  "Given a map from queue name to a number of jobs we are happy to run,
  get some jobs and mark them as running"
  [count-by-queue]
  (let [claim-query (for [[queue-name n] count-by-queue
                          :when (pos? n)]
                      (get-queue-query queue-name n))]
    (when (seq claim-query)
      (db/with-connection [conn]
        (jdbc/atomic
         conn
         (let [jobs-to-claim (db/fetch! {:union claim-query} conn)]
           ;; while we have the read lock let's update them to be in
           (when (seq jobs-to-claim)
             (-> (update :jobs)
                 (sset {:state RUNNING_STATE
                        :started (sql/raw "now() at time zone 'utc'")})
                 (where [:in :id (map :id jobs-to-claim)])
                 (db/execute! conn)))
           (for [job jobs-to-claim]
             (-> job
                 (clojure.core/update :queue-name keyword)
                 (clojure.core/update :args edn/read-string))
             )))))))

(defn job-details [job-id]
  {:pre [(int? job-id)]}

  (-> (select :*)
      (from :jobs)
      (where [:= :id job-id])
      (db/fetch-one!)
      (clojure.core/update :queue-name keyword)
      (clojure.core/update :args edn/read-string)
      (clojure.core/update :state keyword)))

(defn run-jobs [claimed-jobs]
  (let [consumers @consumers]
    (doseq [{job-id :id job-args :args queue-name :queue-name} claimed-jobs]
      (log/info "Running job" job-id " from " queue-name "(args = " job-args ")")
      (let [consumer (:consumer (consumers queue-name))
            ;; at this point we could check for too many jobs and return the job to queue
            ;; if we don't want it.
            state-callback
            (fn [& {:keys [message percent data can-cancel]}]
              ;; can-cancel is a convenience for when there have been
              ;; no side-effects and it is safe to just die

              ;; update the job with progress message and percent, and
              ;; data, which `progress-data` reads back
              (when (or message percent data)
                (-> (update :jobs)
                    (sset (cond-> {}
                            message (assoc :message message)
                            percent (assoc :progress percent)
                            data    (assoc :progress-data (pr-str data))))
                    (where [:= :id job-id])
                    (db/execute!)))
              
              (when (and can-cancel (.isInterrupted (Thread/currentThread)))
                (throw (ex-info "Cancelled" {:cancelled true}))))
            
            run-thread
            (Thread. #(try (let [[new-state message]
                                 (try
                                   (let [result (consumer job-args state-callback)]
                                     (cond
                                       (= :cancelled result) [:cancelled]
                                       (= :error result) [:failed]
                                       :else [:completed]))
                                   (catch Throwable e
                                     (if (:cancelled (ex-data e))
                                       [:cancelled]
                                       (do (log/error e "Running" queue-name job-id job-args)
                                           [:failed (or (.getMessage e) "Unknown cause")]))))]

                             ;; when shutting down, poll-thread puts the job back on the queue
                             (when-not (shutting-down?)
                               (-> (update :jobs)
                                   (sset (cond-> {:state (sql/call :job_state (name new-state))}
                                           message (assoc :message message)))
                                   (where [:= :id job-id])
                                   (db/execute!))
                               (notify-finished queue-name
                                                {:job-id job-id :args job-args
                                                 :state new-state :message message})))
                           (finally 
                             (swap! running-jobs clojure.core/update queue-name dissoc job-id)))
                     (format "Execution of %s job %d" queue-name job-id))
            ]
        (swap! running-jobs assoc-in [queue-name job-id] run-thread)
        (.start run-thread)))))

(defn- process-jobs []
  (let [slots-by-queue (get-spare-slots)
        claimed-jobs (claim-jobs slots-by-queue)]
    
    (when (seq claimed-jobs)
      (log/info "Claimed jobs" claimed-jobs)
      (run-jobs claimed-jobs)))
  
  (let [jobs-by-id (reduce merge (vals @running-jobs))
        ids-to-check (set (keys jobs-by-id))
        ids-to-cancel (when (seq ids-to-check)
                        (-> (update :jobs)
                            (sset {:state CANCELLING_STATE})
                            (where [:and
                                    [:in :id ids-to-check]
                                    [:= :state CANCEL_REQUESTED_STATE]])
                            (returning :id)
                            (db/fetch!)
                            (->> (map :id))))]
    (doseq [id ids-to-cancel]
      (log/info "Cancel" id jobs-by-id)
      (try (.interrupt ^Thread (jobs-by-id id))
           (catch Exception e
             (log/error e "Error when cancelling" id)
             )))))

(defstate poll-thread
  :start
  (let [_ (reset! shutting-down false)
        thread (Thread. #(try
                           (loop []
                             (process-jobs)
                             (Thread/sleep 1000)
                             (recur))
                           (catch InterruptedException e)))
        ]
    (.start thread)
    thread)
  :stop
  (do
    ;; terminate polling
    (.interrupt ^Thread poll-thread)
    (Thread/sleep 50)
    ;; an interrupted job may stop early and finish normally (the network
    ;; model does), but it is to be run again, not to keep its result
    (reset! shutting-down true)
    (let [running-jobs @running-jobs
          restart-jobs (atom #{})]
      (doseq [[queue running-jobs] running-jobs]
        (doseq [[job thread] running-jobs]
          ;; terminate job
          (.interrupt thread)
          (swap! restart-jobs conj job)
          (log/warn "Re-posting" job "on" queue)))
      ;; reset all jobs that were in-progress to READY
      (when (not-empty @restart-jobs)
        (-> (update :jobs)
            (sset {:state READY_STATE})
            (where [:and
                    [:in :id @restart-jobs]
                    [:not [:in :state FINISHED_STATES]]])
            (db/execute!))))))

(defn consume
  "Add a consumer function to the given queue.
  While the queue system is running, this function will be called with
  jobs posted on the queue."
  ([queue-name consumer] (consume queue-name 10 consumer))
  ([queue-name capacity consumer]
   (when-not (zero? capacity)
     (swap! consumers assoc queue-name {:consumer consumer :capacity capacity}))))

(defn enqueue
  "Put a job on `queue-name`. `expected-runtime` is how long in seconds it
  might take at most, which is used to estimate when queued jobs will run."
  [queue-name args & {:keys [expected-runtime]}]
  (-> (insert-into :jobs)
      (values [(cond-> {:queue-name (name queue-name) :args (pr-str args)}
                 expected-runtime (assoc :expected-runtime expected-runtime))])
      (returning :id)
      (db/fetch!)
      (first)
      (:id)))

(defn cancel
  "Cancel job `job-id`. A job still on the queue is cancelled at once. A
  running job's thread is interrupted soon after (see `process-jobs`);
  what that does is up to the job, which may stop early and complete."
  [job-id]
  (-> (update :jobs)
      (sset {:state (sql/call :case
                              [:= :state READY_STATE] CANCELLED_STATE
                              :else CANCEL_REQUESTED_STATE)})
      (where [:and
              [:= :id job-id]
              [:in :state CANCELABLE_STATES]])
      (db/execute!)))

(defn progress-data
  "The latest data job `job-id` gave its progress callback, as :data"
  [job-id]
  {:pre [(int? job-id)]}
  (some-> (select :progress-data)
          (from :jobs)
          (where [:= :id job-id])
          (db/fetch-one!)
          (:progress-data)
          (edn/read-string)))

(defn list-tasks
  "List tasks on all queues, or a given queue"
  ([]
   (-> (select :id :queue-name :state :queued :updated)
       (from :jobs)
       (order-by :queued)
       (db/fetch!)))
  ([queue-name]
   (-> (select :id :queue-name :state :queued :updated)
       (from :jobs)
       (order-by :queued)
       (where [:= :queue-name (name queue-name)])
       (db/fetch!))))

(defn clean-up
  ([queue-name]
   (-> (delete-from :jobs)
       (where [:and
               [:in :state FINISHED_STATES]
               [:= :queue-name (name queue-name)]])
       (db/execute!)))
  ([]
   (-> (delete-from :jobs)
       (where [:in :state FINISHED_STATES])
       (db/execute!))))

(defn erase
  ([queue-name]
   (-> (delete-from :jobs) (where [:= :queue-name (name queue-name)]) (db/execute!)))
  ([]
   (-> (delete-from :jobs) (db/execute!))))

(defn restart [id]
  (-> (update :jobs)
      (sset {:state READY_STATE})
      (where [:= :id id])
      (db/execute!)))

(defn status
  "Get the current state of this job"
  [id]
  (let [state (-> (select :state :queue-name :queued :updated)
                  (from :jobs)
                  (where [:= :id id])
                  (db/fetch!)
                  (first))]
    (when state
      (let [rank (-> (select :%count.*)
                     (from :jobs)
                     (where [:and
                             [:= :queue-name (:queue-name state)]
                             [:< :id id]
                             [:in :state BLOCKING_STATES]])
                     (db/fetch!)
                     (first))]
        (-> state
            (clojure.core/update :queue-name keyword)
            (clojure.core/update :state keyword)
            ;; update state to a keyword?
            (assoc :after rank))))))

(defn restart-all-running!
  "Safe to use only after a system restart, if no other machine is running jobs."
  ([]
   (-> (update :jobs)
       (sset {:state READY_STATE})
       (where [:= :state RUNNING_STATE])
       (db/execute!)))
  ([queue-name]
   (-> (update :jobs)
       (sset {:state READY_STATE})
       (where [:and [:= :queue-name (name queue-name)]
               [:= :state RUNNING_STATE]])
       (db/execute!))))



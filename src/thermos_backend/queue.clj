(ns thermos-backend.queue
  (:require [thermos-backend.db :as db]
            [thermos-backend.config :refer [config]]
            [honeysql.format :refer [format-lock-clause]]
            [honeysql.core :as sql]
            [honeysql-postgres.helpers :refer :all]
            [honeysql-postgres.format]
            [honeysql.helpers :refer :all]
            [jdbc.core :as jdbc]
            [clojure.edn :as edn]
            [clojure.set :refer [map-invert]]
            [mount.core :refer [defstate]]
            ))

(defmethod format-lock-clause :skip-locked [_] "FOR UPDATE SKIP LOCKED")

(def state->label {0 :queued
                   1 :running
                   2 :complete
                   3 :error})

(def label->state (map-invert state->label))

(declare run-one-task)

(defstate queue
  :start
  (let [count (Integer/parseInt (:solver-count config))

        consumers (atom {})

        threads
        (doall
         (for [i (range 0 (or count 1))]
           (Thread. (fn []
                      (try
                        (loop []
                          (run-one-task @consumers)
                          (Thread/sleep 1000)
                          (recur))
                        (catch InterruptedException e))))))
        ]
    (doseq [thread threads] (.start thread))
    (println "Consumer threads started")
    {:poll-threads threads :consumers consumers})
  :stop
  (doseq [thread (:poll-threads queue)]
    (.interrupt thread)))

(defn db->task [task]
  (-> task
      (clojure.core/update :args edn/read-string)
      (clojure.core/update :state state->label)
      (clojure.core/update :queue keyword)))

(defn put
  ([queue task]
   (db/with-connection [conn]
     (-> (insert-into :jobs)
         (values [{:queue (name queue)
                   :args (pr-str task)
                   :state (label->state :queued)}])
         (returning :id)
         (sql/format)
         (->> (jdbc/fetch conn))
         (first)
         :id))))

(defn consume
  ([queue-name handler]
   (swap! (:consumers queue) assoc queue-name handler)))

(defn- claim-job [conn consumers]
  (let [qs (vec (map name (keys consumers)))]
    (when-not (empty? qs)
      (when-let [out (-> (select :id :queue :args :state)
                         (from :jobs)
                         (where [:and
                                 [:= :state 0]
                                 [:in :queue qs]])
                         (lock :mode :skip-locked)
                         (limit 1)
                         (sql/format)
                         (->> (jdbc/fetch conn))
                         (first))]
        (db->task out)
        ))))

(defn- set-state [conn job state]
  (-> (update :jobs)
      (sset {:state (label->state state)})
      (where [:= :id (:id job)])
      (sql/format)
      (->> (jdbc/execute conn))))

(defn- run-one-task [consumers]
  (db/with-connection [conn]
    (when-let [[job consumer]
               (jdbc/atomic
                conn
                (let [job (claim-job conn consumers)
                      consumer (consumers (:queue job))]
                  (when (and job consumer)
                    (set-state conn job :running)
                    [job consumer])
                  ))]
      (println "About to run" job)
      (try (jdbc/atomic
            conn
            (consumer conn (:args job))
            (set-state conn job :complete)
            (println job "Job completed"))
           (catch Exception e
             (println "Job failed" job e)
             (set-state conn job :error))))))

(defn ls []
  (db/with-connection [c]
    (-> (select :id :queue :args :state)
        (from :jobs)
        (sql/format)
        (->> (jdbc/fetch c)
             (map db->task))
        )))

(defn status [job-id]
  (db/with-connection [c]
    (let [result (-> (select :id :queue :args :state)
                     (from :jobs)
                     (where [:= :id job-id])
                     (sql/format)
                     (->> (jdbc/fetch c))
                     (first)
                     (db->task))

          position (when result
                     (-> (select :%count.*)
                         (from :jobs)
                         (where [:and
                                 [:= :queue (name (:queue result))]
                                 [:< :id (:id result)]
                                 [:or
                                  [:= :state (label->state :queued)]
                                  [:= :state (label->state :running)]]])
                         (sql/format)
                         (->> (jdbc/fetch c))
                         (first)))
          ]
      
      (when result
        (assoc result :after (:count position)))
      )))


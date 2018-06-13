(ns thermos-ui.backend.queue
  (:require [thermos-ui.backend.db :as db]
            [honeysql.format :refer [format-lock-clause]]
            [honeysql.core :as sql]
            [honeysql-postgres.helpers :refer :all]
            [honeysql-postgres.format]
            [honeysql.helpers :refer :all]
            [jdbc.core :as jdbc]
            [clojure.edn :as edn]
            [clojure.set :refer [map-invert]]
            [com.stuartsierra.component :as component]
            ))

(defmethod format-lock-clause :skip-locked [_] "FOR UPDATE SKIP LOCKED")

(def state->label {0 :queued
                   1 :running
                   2 :complete
                   3 :error})

(def label->state (map-invert state->label))

(declare run-one-task)

(defrecord Queue [database poll-thread consumers]
  component/Lifecycle

  (start [component]
    (let [consumers (atom {})

          thread (Thread. (fn []
                            (try
                              (loop []
                                (run-one-task (:database component) @consumers)
                                (Thread/sleep 1000)
                                (recur))
                              (catch InterruptedException e))))
          ]
      (.start thread)
      (println "Consumer thread started")
      (assoc component
             :poll-thread thread
             :consumers consumers)))
  
  (stop [component]
    (.interrupt (:poll-thread component))
    (assoc component
           :poll-thread nil
           :consumers nil)))

(defn new-queue []
  (map->Queue {}))

(defn task->db [task]
  
  )

(defn db->task [task]
  (-> task
      (clojure.core/update :args edn/read-string)
      (clojure.core/update :state state->label)
      (clojure.core/update :queue keyword)))

(defn put
  ([q queue task]
   (db/with-connection [conn (:database q)]
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
  ([q queue handler]
   (swap! (:consumers q) assoc queue handler)))

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

(defn- set-state [conn id state]
  (-> (update :jobs)
      (sset {:state state})
      (where [:= :id id])
      (sql/format)
      (->> (jdbc/execute conn))))

(defn- run-one-task [db consumers]
  (db/with-connection [conn db]
    (when-let [job (claim-job conn consumers)]
      (let [args (:args job)
            queue (:queue job)]
          (when-let [consumer (consumers queue)]
            (println "About to run" job)
            (set-state conn (:id job) (label->state :running))
            ;; issue checkpoint here
            (jdbc/atomic conn
             (try
               (consumer conn args)
               (catch Exception e
                 (println e)
                 (set-state conn (:id job) (label->state :error)))))
            (set-state conn (:id job) (label->state :complete))))
      )))


(defn ls [queue]
  (db/with-connection [c (:database queue)]
    (-> (select :id :queue :args :state)
        (from :jobs)
        (sql/format)
        (->> (jdbc/fetch c)
             (map db->task))
        )))

(defn status [queue job-id]
  (db/with-connection [c (:database queue)]
    (-> (select :id :queue :args :state)
        (from :jobs)
        (where [:= :id job-id])
        (sql/format)
        (->> (jdbc/fetch c))
        (first)
        (db->task))))


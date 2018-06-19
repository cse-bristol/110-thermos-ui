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

(defrecord Queue [database poll-threads consumers count]
  component/Lifecycle

  (start [component]
    (let [consumers (atom {})

          threads
          (doall
           
           (for [i (range 0 (or count 1))]
             (Thread. (fn []
                        (try
                          (loop []
                            (run-one-task (:database component) @consumers)
                            (Thread/sleep 1000)
                            (recur))
                          (catch InterruptedException e))))))
          ]
      (doseq [thread threads] (.start thread))
      (println "Consumer threads started")
      (assoc component
             :poll-threads threads
             :consumers consumers)))
  
  (stop [component]
    (doseq [thread (:poll-threads component)]
      (.interrupt thread))
    
    (assoc component
           :poll-threads nil
           :consumers nil)))

(defn new-queue [config]
  (map->Queue {:count
               (Integer/parseInt (:solver-count config))}))

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


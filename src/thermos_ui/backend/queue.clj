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
            ))

(defmethod format-lock-clause :skip-locked [_] "FOR UPDATE SKIP LOCKED")

(def state->label {0 :queued
                   1 :running
                   2 :complete
                   3 :error})

(def label->state (map-invert state->label))

(defn put
  ([task]
   (put :default task))
  
  ([queue task]
   (with-open [conn (db/connection)]
     (-> (insert-into :jobs)
         (values [{:queue (name queue)
                   :args (pr-str task)
                   :state (label->state :queued)}])
         (returning :id)
         (sql/format)
         (->> (jdbc/fetch conn))
         (first)
         :id))))

(def consumers (atom {}))

(defn consume
  ([handler]
   (consume :default handler))
  ([queue handler]
   (swap! consumers assoc queue handler)))

(defn- claim-job [conn]
  (let [qs (vec (map name (keys @consumers)))]
    (when-not (empty? qs)
      (when-let [out (-> (select :id :queue :args)
                         (from :jobs)
                         (where [:and
                                 [:= :state 0]
                                 [:in :queue qs]])
                         (lock :mode :skip-locked)
                         (limit 1)
                         (sql/format)
                         (->> (jdbc/fetch conn))
                         (first))]
        (-> out
            (clojure.core/update :queue keyword)
            (clojure.core/update :state state->label))
        ))))

(defn- set-state [conn id state]
  (-> (update :jobs)
      (sset {:state state})
      (where [:= :id id])
      (sql/format)
      (->> (jdbc/execute conn))))

(defn- run-one-task []
  (with-open [conn (db/connection)]
    (jdbc/atomic
     conn
     (when-let [job (claim-job conn)]
       (try
         (let [args (edn/read-string (:args job))
               queue (:queue job)]

           (when-let [consumer (@consumers queue)]
             (println "About to run" job)
             (set-state conn (:id job) (label->state :running))
             ;; issue checkpoint here?
             (consumer conn args)
             (set-state conn (:id job) (label->state :complete))))
         (catch Exception e
           
             ;; mark job failed and log failure
             ;; rollback checkpoint here
             (set-state conn (:id job) (label->state :error))
           ))))))

(defn start-consumer-thread! []
  (let [t (Thread. (fn []
                     (try
                       (loop []
                         (run-one-task)
                         (Thread/sleep 1000)
                         (recur))
                       (catch InterruptedException e
                         
                         ))))]
    (.start t)))

(defn cancel [task-id]
  ;; There's no method to signal about this out-of-band that I can
  ;; think of, apart from making another table of death signals
  ;; and polling that from another thread. We can do that later.
  )

(defn ls []
  (with-open [c (db/connection)]
    (-> (select :id :queue :args :state)
        (from :jobs)
        (sql/format)
        (->> (jdbc/fetch c)
             (map #(-> %
                  (clojure.core/update :state state->label)
                  (clojure.core/update :queue keyword))))
        )))


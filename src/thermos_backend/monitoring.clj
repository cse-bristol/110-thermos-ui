(ns thermos-backend.monitoring
  "Some http endpoints for monitoring THERMOS using prometheus"
  (:require [clojure.string :as string]
            [thermos-backend.queue :as queue]
            [thermos-backend.db.users :as users]
            [thermos-backend.db.projects :as projects]
            [thermos-backend.db :as db]))

(defn- clean-name [k]
  (.replaceAll (name k) "-" "_"))

(defn- format-labels [m]
  (let [m (dissoc m :name :value)]
    (when (seq m)
      (str "{"
           (string/join ", "
                        (for [[k v] m]
                          (str (clean-name k) " = " (pr-str v))))
           "}")
      )))

(defn- format-metrics
  "Format some metrics into the prometheus exposition format.

  See https://prometheus.io/docs/instrumenting/exposition_formats/ for the format.

  Each metric is a map having :name and :value at least.
  If the metric has other kvs they are put in as labels.  
  "
  [metrics & {:keys [types doc]}]
  
  (str (string/join
        "\n"
        (flatten
         (for [[metric-name metrics] (group-by :name metrics)]
           
           (let [type-info (get types metric-name :gauge)
                 type-doc (get doc metric-name "Undocumented")
                 metric-name (clean-name metric-name)]
             [(str "# HELP " metric-name " " type-doc)
              (str "# TYPE " metric-name " " (name type-info))
              (for [metric metrics]
                (str metric-name (format-labels metric) " " (:value metric))
                )]))))
       "\n"))

(defn- system-metrics
  "Gather some metrics about the system in a format suitable for format-metrics to use."
  []

  (let [queue-states (frequencies
                      (map (juxt :queue-name :state)
                           (queue/list-tasks)))]
    (concat
     (for [[[queue state] n] queue-states]
       {:name :queue-count
        :value n
        :queue-name queue
        :task-state state})
     (for [table [:users :projects :maps :candidates]]
       {:name :object-count
        :value (db/count-rows table)
        :object (name table)}))))

(defn formatted-metrics []
  (format-metrics
   (system-metrics)
   :doc {:queue-count "The number of entries in each queue"
           :object-count "The number of rows in database tables"}))

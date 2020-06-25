(ns thermos-backend.config
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [mount.core :refer [defstate]]))

(def ^:private values (atom nil))

(defn- keywordize [s]
  (-> (str/lower-case s)
      (str/replace "_" "-")
      (str/replace "." "-")
      (keyword)))

(defn- sanitize-key [k]
  (let [s (keywordize (name k))]
    (if-not (= k s) (log/warn "Warning: environ key" k "has been corrected to" s))
    s))

(defn- sanitize-val [k v]
  (if (string? v)
    v
    (do (log/warn "Warning: environ value" (pr-str v) "for key" k "has been cast to string")
        (str v))))

(defn- clean-values [vals]
  (into {} (for [[k v] vals] [(sanitize-key k) (sanitize-val k v)])))

(defn- system-environment []
  (->> (System/getenv)
       (map (fn [[k v]] [(keywordize k) v]))
       (into {})))

(defn- system-properties []
  (->> (System/getProperties)
       (map (fn [[k v]] [(keywordize k) v]))
       (into {})))

(defn- read-strings [config defaults]
  (reduce-kv
   (fn [a k v]
     (if (and (string? v)
              (contains? defaults k)
              (not (string? (defaults k))))
       (let [default-v (defaults k)]
         (println "Parsing config entry" k)
         (assoc a k
                (cond
                  (boolean? default-v)
                  (#{"1" "true" "yes"} (.toLowerCase v))

                  (double? default-v)
                  (Double/parseDouble v)

                  (int? default-v)
                  (Integer/parseInt v)

                  :else
                  (edn/read-string v))))

       (assoc a k v)))
   {} config))

(defn load-values []
  (let [defaults
        (some->> (io/resource "config.edn")
                 (io/reader)
                 (slurp)
                 (edn/read-string))]
    (->> (merge
          defaults
          (select-keys (system-environment) (keys defaults))
          (select-keys (system-properties) (keys defaults)))
         (read-strings defaults)
         )))

(defstate config
  :start (load-values))

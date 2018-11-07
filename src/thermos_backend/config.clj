(ns thermos-backend.config
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            ))

(def ^:private values (atom nil))

(defn- keywordize [s]
  (-> (str/lower-case s)
      (str/replace "_" "-")
      (str/replace "." "-")
      (keyword)))

(defn- sanitize-key [k]
  (let [s (keywordize (name k))]
    (if-not (= k s) (println "Warning: environ key" k "has been corrected to" s))
    s))

(defn- sanitize-val [k v]
  (if (string? v)
    v
    (do (println "Warning: environ value" (pr-str v) "for key" k "has been cast to string")
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

(defn load-values []
  (let [defaults
        (some->> (io/resource "config.edn")
                 (io/reader)
                 (slurp)
                 (edn/read-string)
                 (clean-values))]
    (merge
     defaults
     (select-keys (system-environment) (keys defaults))
     (select-keys (system-properties) (keys defaults)))))


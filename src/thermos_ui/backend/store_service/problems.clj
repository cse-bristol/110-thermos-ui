(ns thermos-ui.backend.store-service.problems
   (:require [clojure.java.io :as io]
             [clojure.string :refer [join]]
             [clojure.string :as s]
             [environ.core :refer [env]]
             [clojure.edn :as edn]))

;;Storage of problems - id will be hash of stored thing a la github
(defonce server-url "http://localhost:3449")
(defonce store-location (env :problem-store))
(defonce file-ext ".edn")

(defn- create-problem-response [org name id problem]
  "Returna problem response"
  {:location (.getPath problem)
   :org org
   :name name
   :id id})

(defn- create-problem-file [org name id]
  (io/as-file
   (str (join "/" [store-location org name ""])
        id
        file-ext)))

(defn store [org name problem]
  "Store the given problem in the file system under org/name/:version where version is a hash of the org/name/problem"
  (let [hash (hash [org name (slurp problem)])
        file (create-problem-file org name hash)]
    (if (not (.exists file))
      (do
        (io/make-parents file)
        (io/copy problem (io/file file))))
    (create-problem-response org name hash file)))

(defn getone [org name id]
  (let [problem-file (create-problem-file org name id)]
    (let [f (.getPath problem-file)
          edn  (with-open [pbr (java.io.PushbackReader. (io/reader f))]
                 (read pbr false nil))]
    (assoc
     (create-problem-response org name id problem-file)
     :problem edn))))

(defn gather
  ([org-key] (gather org-key nil))
  ([org-key name]
   (let [org-path (if (nil? name)
                    (io/as-file (join "/" [store-location org-key]))
                    (io/as-file (join "/" [store-location org-key name])))
        fs (file-seq org-path)
        file-map (reduce (fn[accum f]
                           (if (.isFile f)
                             (let [id (s/replace (.getName f) file-ext "")
                                   location (-> (.getPath f)
                                                (s/replace-first  #".+\/problems\/" "")
                                                (#(s/join "/" [server-url "problem" %])))]
                               (assoc accum (keyword id) {:location location
                                                          :id id}))
                             accum))
                         {}
                         fs)]
      file-map)))

(defn delete [org name id]
  (let [f (create-problem-file org name id)]
    (if (.exists f)
      (io/delete-file f true)
      false)))

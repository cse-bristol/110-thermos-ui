(ns thermos-ui.store-service.store.problems
   (:require [clojure.java.io :as io]
             [clojure.string :refer [join]]
             [clojure.string :as s]
             [environ.core :refer [env]]))

;;Storage of problems - id will be hash of stored thing a la github
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
    (create-problem-response org name id (create-problem-file org name id)))

(defn gather [org-key]
  "Return problems under org-key."
  (let [org-path (io/as-file (join "/" [store-location org-key]))
        fs (file-seq org-path)
        file-map (reduce (fn[accum f]
                           (if (.isFile f)
                             (let [id (s/replace (.getName f) file-ext "")
                                   location (-> (.getPath f)
                                                (s/replace-first  #".+\/problems\/" "")
                                                (#(s/join "/" ["problem" %])))]
                               (assoc accum (keyword id) {:location location
                                                          :id id}))
                             accum))
                         {}
                         fs)]
      file-map))

(defn delete [org name id]
  (io/delete-file (create-problem-file org name id) false))

(-> 1
    (+ 1))

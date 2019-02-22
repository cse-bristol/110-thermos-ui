(ns thermos-backend.current-uri)

(def ^:dynamic *current-uri* nil)

(defn wrap-current-uri [h]
  (fn [r]
    (binding [*current-uri* (:uri r)]
      (h r))))

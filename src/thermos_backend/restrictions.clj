(ns thermos-backend.restrictions
  (:require [thermos-backend.config :refer [config]]
            [thermos-backend.db.users :as users]
            [thermos-backend.db.projects :as projects]))


(defn get-restriction-info 
  "Facts about the restrictions that restricted users face,
   and how much this particular user/project has used up their
   allowances."
  ([user]
   (let [restricted-user? (= :restricted (:auth user))]
     {:max-restricted-jobs-per-week (config :max-restricted-jobs-per-week)
      :max-restricted-project-runtime (config :max-restricted-project-runtime)
      :max-restricted-gis-features (config :max-restricted-gis-features)
      :user-jobs-run-in-week (users/jobs-since (:id user) 7)
      :restricted-user? restricted-user?
      :restricted? restricted-user?
      :num-gis-features (users/num-gis-features (:id user))}))
  
  ([user project-id]
   {:pre [(int? project-id)]}
   (let [restricted-project? (projects/is-restricted-project? project-id)
         restricted-user? (= :restricted (:auth user))
         restricted? (or restricted-project? restricted-user?)]
     (merge
      (get-restriction-info user)
      {:project-jobs-run-in-week (projects/jobs-since project-id 7)
       :restricted-project? restricted-project?
       :restricted? restricted?}))))

(defn exceeded-gis-feature-count? [user]
  (let [restricted-user? (= :restricted (:auth user))
        max-restricted-gis-features (config :max-restricted-gis-features)
        num-gis-features (users/num-gis-features (:id user))]
    
    (if restricted-user?
      (> num-gis-features max-restricted-gis-features)
      false)))

(defn exceeded-jobs-per-week? [user project-id]
  {:pre [(int? project-id)]}
  (let [user-jobs-run-in-week (users/jobs-since (:id user) 7)
        project-jobs-run-in-week (projects/jobs-since project-id 7)
        max-restricted-jobs-per-week (config :max-restricted-jobs-per-week)]

    (or (and (= :restricted (:auth user))
             (>= user-jobs-run-in-week max-restricted-jobs-per-week))
        (and (projects/is-restricted-project? project-id)
             (>= project-jobs-run-in-week max-restricted-jobs-per-week)))))
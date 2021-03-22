(ns thermos-backend.restrictions
  (:require [thermos-backend.config :refer [config]]
            [thermos-backend.db.users :as users]
            [thermos-backend.db.projects :as projects]))


(defn get-restriction-info
  "Facts about the restrictions that restricted users face,
   and how much this particular user/project has used up their
   allowances."
  ([user]
   (let [auth (:auth user)
         max-restricted-jobs-per-week (auth (config :max-restricted-jobs-per-week))
         max-restricted-project-runtime (auth (config :max-restricted-project-runtime))
         max-restricted-gis-features (auth (config :max-restricted-gis-features))
         priority-queue-weight (auth (config :priority-queue-weight))]
     {:max-restricted-jobs-per-week max-restricted-jobs-per-week
      :max-restricted-project-runtime max-restricted-project-runtime
      :max-restricted-gis-features max-restricted-gis-features
      :priority-queue-weight priority-queue-weight
      :contact-email (config :contact-email)
      :user-jobs-run-in-week (users/jobs-since (:id user) 7)
      :has-restrictions? (or (some? max-restricted-jobs-per-week)
                             (some? max-restricted-project-runtime)
                             (some? max-restricted-gis-features)
                             (some? priority-queue-weight))
      :num-gis-features (users/num-gis-features (:id user))
      :user-auth (:auth user)}))

  ([user project-id]
   {:pre [(int? project-id)]}
   (let [project-auth (projects/most-permissive-project-user-auth project-id)
         user-auth (:auth user)
         auth (users/most-permissive user-auth project-auth)]
     (merge
      (get-restriction-info user)
      {:project-jobs-run-in-week (projects/jobs-since project-id 7)
       :project-auth project-auth
       :auth auth}))))

(defn exceeded-gis-feature-count? [user]
  (let [auth (:auth user)
        max-restricted-gis-features (auth (config :max-restricted-gis-features))
        num-gis-features (when max-restricted-gis-features
                           (users/num-gis-features (:id user)))]

    (if max-restricted-gis-features
      (> num-gis-features max-restricted-gis-features)
      false)))

(defn exceeded-jobs-per-week? [user project-id]
  {:pre [(int? project-id)]}
  (let [auth (:auth user)
        project-auth
        (projects/most-permissive-project-user-auth project-id)

        user-max-restricted-jobs-per-week
        (auth (config :max-restricted-jobs-per-week))

        project-max-restricted-jobs-per-week
        (project-auth (config :max-restricted-jobs-per-week))

        user-jobs-run-in-week
        (when user-max-restricted-jobs-per-week
          (users/jobs-since (:id user) 7))

        project-jobs-run-in-week
        (when project-max-restricted-jobs-per-week
          (projects/jobs-since project-id 7))]

    (or (and (some? user-max-restricted-jobs-per-week)
             (>= user-jobs-run-in-week user-max-restricted-jobs-per-week))
        (and (some? project-max-restricted-jobs-per-week)
             (>= project-jobs-run-in-week project-max-restricted-jobs-per-week)))))

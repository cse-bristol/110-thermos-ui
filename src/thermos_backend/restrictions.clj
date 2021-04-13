;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

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
         user-max-jobs-per-week (auth (config :max-jobs-per-week))
         max-project-runtime (auth (config :max-project-runtime))
         max-gis-features (auth (config :max-gis-features))
         priority-queue-weight (auth (config :priority-queue-weight))]
     {:user-max-jobs-per-week user-max-jobs-per-week
      :max-project-runtime max-project-runtime
      :max-gis-features max-gis-features
      :priority-queue-weight priority-queue-weight
      :contact-email (config :contact-email)
      :user-jobs-run-in-week (users/jobs-since (:id user) 7)
      :has-restrictions? (or (some? user-max-jobs-per-week)
                             (some? max-project-runtime)
                             (some? max-gis-features)
                             (some? priority-queue-weight))
      :num-gis-features (users/num-gis-features (:id user))
      :user-auth (:auth user)}))

  ([user project-id]
   {:pre [(int? project-id)]}
   (let [project-auth (projects/most-permissive-project-user-auth project-id)
         max-project-runtime (project-auth (config :max-project-runtime))
         project-max-jobs-per-week (project-auth (config :max-jobs-per-week))]
     (merge
      (get-restriction-info user)
      {:project-jobs-run-in-week (projects/jobs-since project-id 7)
       :max-project-runtime max-project-runtime
       :project-max-jobs-per-week project-max-jobs-per-week
       :project-auth project-auth}))))

(defn exceeded-gis-feature-count? [user]
  (let [auth (:auth user)
        max-gis-features (auth (config :max-gis-features))
        num-gis-features (when max-gis-features
                           (users/num-gis-features (:id user)))]

    (if max-gis-features
      (> num-gis-features max-gis-features)
      false)))

(defn exceeded-jobs-per-week? [user project-id]
  {:pre [(int? project-id)]}
  (let [auth (:auth user)
        project-auth
        (projects/most-permissive-project-user-auth project-id)

        user-max-jobs-per-week
        (auth (config :max-jobs-per-week))

        project-max-jobs-per-week
        (project-auth (config :max-jobs-per-week))

        user-jobs-run-in-week
        (when user-max-jobs-per-week
          (users/jobs-since (:id user) 7))

        project-jobs-run-in-week
        (when project-max-jobs-per-week
          (projects/jobs-since project-id 7))]

    (or (and (some? user-max-jobs-per-week)
             (>= user-jobs-run-in-week user-max-jobs-per-week))
        (and (some? project-max-jobs-per-week)
             (>= project-jobs-run-in-week project-max-jobs-per-week)))))

(ns thermos-pages.restriction-components)


(defn show-user-restrictions [restriction-info & {:keys [as-card]}]
  (when (:has-restrictions? restriction-info)
    (let [{:keys [user-auth
                  project-auth
                  user-jobs-run-in-week
                  project-jobs-run-in-week
                  num-gis-features
                  max-restricted-jobs-per-week
                  max-restricted-gis-features
                  max-restricted-project-runtime
                  priority-queue-weight
                  contact-email]} restriction-info]
      [:div {:class (when as-card :card)}
       [:p "Your user account level: " [:b (name user-auth)]]
       (when (not= user-auth project-auth)
         [:p "Project level: " [:b (name project-auth)]])

       [:ul
        (when priority-queue-weight
          [:li "Your optimisations will only be run once there are no non-restricted projects waiting to run."])

        (when max-restricted-gis-features
          [:li "You cannot load more than " [:b max-restricted-gis-features] " buildings or roads across all the projects you are part of. "
           [:b num-gis-features "/" max-restricted-gis-features] " buildings and roads loaded."])

        (when max-restricted-jobs-per-week
          [:li "You cannot run more than " [:b max-restricted-jobs-per-week] " optimisations across all projects per 7-day period. "
           [:b user-jobs-run-in-week "/" max-restricted-jobs-per-week] " optimisations run so far."])

        (when (and (some? project-jobs-run-in-week) max-restricted-jobs-per-week)
          [:li "No-one can run more than " [:b max-restricted-jobs-per-week] " optimisations on this project per 7-day period. "
           [:b project-jobs-run-in-week "/" max-restricted-jobs-per-week] " optimisations run so far."])

        (when max-restricted-project-runtime
          [:li "Optimisations will automatically finish after " [:b max-restricted-project-runtime] " hour(s)."])]
       [:p "If you think you should have a different user account level, or if you would like to upgrade your account, "
        "please contact " [:a {:href (str "mailto:" contact-email)} contact-email] "."]])))

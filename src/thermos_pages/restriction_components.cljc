(ns thermos-pages.restriction-components)


(defn show-user-restrictions [restriction-info & {:keys [as-card]}]
  (when (:has-restrictions? restriction-info)
    (let [{:keys [user-auth
                  project-auth
                  user-jobs-run-in-week
                  project-jobs-run-in-week
                  num-gis-features
                  user-max-jobs-per-week
                  project-max-jobs-per-week
                  max-gis-features
                  max-project-runtime
                  priority-queue-weight
                  contact-email]} restriction-info
          project-auth-name (and project-auth (name project-auth))
          user-auth-name (and user-auth (name user-auth))
          ]
      [:div {:class (when as-card :card)}
       [:p "Your user account level: " [:b user-auth-name]]
       (when (and some? project-auth (not= user-auth project-auth))
         [:p "Project level: " [:b project-auth-name]])

       [:ul
        (when priority-queue-weight
          [:li [:b user-auth-name] " user account: Your optimisations "
           "will only be run once there are no non-restricted projects waiting to run."])

        (when max-gis-features
          [:li [:b user-auth-name] " user account: You cannot load more than " 
           [:b max-gis-features] " buildings or roads across all the projects you are part of. "
           [:b num-gis-features "/" max-gis-features] " buildings and roads loaded."])

        (when user-max-jobs-per-week
          [:li [:b user-auth-name] " user account: You cannot run more than " 
           [:b user-max-jobs-per-week] " optimisations across all projects per 7-day period. "
           [:b user-jobs-run-in-week "/" user-max-jobs-per-week] " optimisations run so far."])

        (when project-max-jobs-per-week
          [:li  [:b project-auth-name] " project: No-one can run more than " 
           [:b project-max-jobs-per-week] " optimisations on this project per 7-day period. "
           [:b project-jobs-run-in-week "/" project-max-jobs-per-week] " optimisations run so far."])

        (when max-project-runtime
          [:li [:b project-auth-name] " project: Optimisations will automatically finish after " 
           [:b max-project-runtime] " hour(s)."])]
       
       [:p "If you think you should have a different user account level, or if you would like to upgrade your account, "
        "please contact " [:a {:href (str "mailto:" contact-email)} contact-email] "."]])))

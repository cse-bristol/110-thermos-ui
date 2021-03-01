(ns thermos-pages.restriction-components)


(defn show-user-restrictions [restriction-info & {:keys [as-card]}]
  (when (:restricted-user? restriction-info)
    (let [max-restricted-jobs-per-week (:max-restricted-jobs-per-week restriction-info)
          user-jobs-run-in-week (:user-jobs-run-in-week restriction-info)
          project-jobs-run-in-week (:project-jobs-run-in-week restriction-info)
          num-gis-features (:num-gis-features restriction-info)
          max-restricted-gis-features (:max-restricted-gis-features restriction-info)
          max-restricted-project-runtime (:max-restricted-project-runtime restriction-info)]
      [:div {:class (when as-card :card)}
       [:p "You currently have a trial user account:"]
       [:ul
        [:li "Your optimisations will only be run once there are no non-restricted projects waiting to run."]
        [:li "You cannot load more than " max-restricted-gis-features " buildings or roads across all the projects you are part of. "
         [:b num-gis-features "/" max-restricted-gis-features] " buildings and roads loaded."]

        [:li "You cannot run more than " max-restricted-jobs-per-week " optimisations across all projects per 7-day period. "
         [:b user-jobs-run-in-week "/" max-restricted-jobs-per-week] " optimisations run so far."]

        (when project-jobs-run-in-week
          [:li "No-one can run more than " max-restricted-jobs-per-week " optimisations on this project per 7-day period. "
           [:b project-jobs-run-in-week "/" max-restricted-jobs-per-week] " optimisations run so far."])

        [:li "Optimisations will automatically finish after " max-restricted-project-runtime " hours."]]
       [:p ]
       
       [:p ]])))
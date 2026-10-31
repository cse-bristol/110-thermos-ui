;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.pages.user-settings
  (:require [thermos-backend.pages.common :refer [page]]
            [thermos-pages.common :refer [style]]))

(defn settings-page [user]
  (page {:title "Account settings" :js ["/js/push.js"]}
        [:form {:method :POST}
         [:div
          [:div [:b "Email address: "] [:span (:id user)]]
          [:div [:b "User type: "] [:span (:auth user)]]
          [:div [:b "Name: "]
           [:input.text-input {:type :text :value (:name user) :name :new-name}]]
          [:div [:b "New password: "]
           [:input.text-input {:type :password :name :password-1}]]
          [:div [:b "Repeat password: "]
           [:input.text-input {:type :password :name :password-2}]]
          [:div
           [:label [:b "System messages by email: "]
            [:input (cond->
                        {:type :checkbox
                         :name :system-messages}
                      (:system-messages user)
                      (assoc :checked :checked) ;; I hate html5
                      )]]
           [:div "If you check this box, the system may send you emails about:"
            [:ul
             [:li "System updates that fix bugs or change how THERMOS works."]
             [:li "Planned or accidental maintenance periods when the system will be offline."]]]]

          [:div
           [:label [:b "Emails when jobs finish: "]
            [:input (cond->
                        {:type :checkbox
                         :name :job-emails}
                      (:job-emails user)
                      (assoc :checked :checked))]]
           [:div "If you check this box, we will email you when a model run or map import that you started finishes."]]

          [:div.push-settings
           [:b "Browser notifications: "]
           [:span {:data-push-state "unsupported" :hidden true}
            "This browser can't show notifications."]
           [:span {:data-push-state "denied" :hidden true}
            "Notifications from THERMOS are blocked in this browser's site settings."]
           [:span {:data-push-state "off" :hidden true}
            "Off in this browser. "
            [:button.button {:type :button :data-push-action "enable"} "Turn on"]]
           [:span {:data-push-state "on" :hidden true}
            "On in this browser. "
            [:button.button {:type :button :data-push-action "test"} "Send a test"]
            [:button.button {:type :button :data-push-action "disable"} "Turn off"]]
           [:span.push-message {:style (style :margin-left :1em)}]
           [:div "These pop up in this browser when a model run or map import that you started finishes, even if THERMOS isn't open. "
            "Turn them on in each browser where you want them."]]

          [:div "We won't send you any emails that are not about THERMOS, and we won't tell anyone else anything about you."]
          
          [:div [:input.button {:type :submit :value "Save"}]]]]))



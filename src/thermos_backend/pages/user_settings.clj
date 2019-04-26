(ns thermos-backend.pages.user-settings
  (:require [thermos-backend.pages.common :refer [page]]
            [thermos-pages.common :refer [style]]))

(defn settings-page [user]
  (page {:title "Account settings"}
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
             [:li "Planned or accidental maintenance periods when the system will be offline."]
             [:li "The completion or failure of long-running processes that you have started."]]]

           [:div "We won't send you any emails that are not about THERMOS, and we won't tell anyone else anything about you."]]
          
          [:div [:input.button {:type :submit :value "Save"}]]]]))



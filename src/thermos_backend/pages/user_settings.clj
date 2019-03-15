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
          
          [:div [:input.button {:type :submit :value "Save"}]]]]))



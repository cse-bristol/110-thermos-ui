(ns thermos-backend.pages.login-form
  (:require [thermos-backend.pages.common :refer [page]]))

(defn login-form [redirect-target flash]
  (page
   {:title "Login"}
   [:form {:method "POST"}
    [:div
     [:label "Username: "
      [:input
       {:name :username
        :type :text
        :id :username
        :placeholder "your@email.com"}]]
     [:label "Password: "
      [:input {:type :password :name :password :id :password} ]]]
    [:div
     [:input {:type :submit :value "Login" :name :login}]
     [:input {:type :submit :value "Forgot" :name :forgot}]
     [:input {:type :submit :value "Create" :name :create}]]]))


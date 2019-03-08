(ns thermos-backend.pages.login-form
  (:require [thermos-backend.pages.common :refer [page]]
            [thermos-pages.common :refer [style]]))

(defn login-form [redirect-target flash]
  (page
   {:title "Login"}
   [:form {:method "POST"
           :style (style :display :flex)}
    [:div {:style (style :margin :auto)}
     [:div {:style (style :padding :0.5em)}
      [:label "Username: "
       [:input.text-input
        {:name :username
         :type :text
         :id :username
         :placeholder "your@email.com"}]]]
     [:div {:style (style :padding :0.5em)}
      [:label "Password: "
       [:input.text-input
        {:type :password :name :password :id :password} ]]]
     [:div
      [:input.button
       {:style (style :margin :0.1em)
        :type :submit :value "Login" :name :login}]
      [:input.button
       {:style (style :margin :0.1em)
        :type :submit :value "Recover" :name :forgot}]
      [:input.button
       {:style (style :margin :0.1em)
        :type :submit :value "Create" :name :create}]]]]))


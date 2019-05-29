(ns thermos-backend.pages.login-form
  (:require [thermos-backend.pages.common :refer [page]]
            [thermos-pages.common :refer [style]]))

(defn login-form [redirect-target flash]
  (page
   {:title "Login"}
   [:form {:method "POST"
           :style (style :display :flex)}
    [:div {:style (style :margin :auto)}
     (case (keyword flash)
       :check-mail
       [:div "A password recovery email has been sent. Check your email."]
       :exists
       [:div "That user already exists. If it is your email address, and you have forgotten your password, click recover."]
       :failed
       [:div "Login failed"]
       nil)
     
     
     [:div {:style (style :padding :0.5em)}
      [:label "Username: "
       [:input.text-input
        {:name :username
         :type :email
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
      ]
     [:div
      [:p "If you do not have an account, you can sign up."]
      [:p "You can give your email address as your username - if you do, we may use it for:"]
      [:ol
        [:li "Telling you about changes or maintenance to THERMOS"]
        [:li "Asking you about how you are using the application or why you are interested in it"]]
      [:p "If you do not use your email address, that's fine, but you won't be able to recover your password."]
      [:p "You can read more about " [:a {:href "/help/data-protection.html"} "how THERMOS uses your data"]]
      
      [:input.button
       {:style (style :margin :0.1em)
        :type :submit :value "Create" :name :create}]
      ]
     ]]))


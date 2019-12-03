(ns thermos-backend.pages.login-form
  (:require [thermos-backend.pages.common :refer [page]]
            [thermos-pages.common :refer [style]]))

(defn login-form [redirect-target flash]
  (page
   {:title "Login"
    :body-style {:display :flex :flex-direction :column}}
   [:div {:style (style :display :flex
                        :margin :1em
                        :flex-grow 1
                        :max-width "100%")}
    
    [:form {:method "POST"
            :style (style :margin :auto)}
     
     [:div.flex-cols
      [:div {:style (style :padding :0.5em :flex-grow 1)}
       [:label "Username: "
        [:input.text-input
         {:name :username
          :type :text
          :style (style :width :100%)
          :width :100%
          :required :required
          :pattern ".{5,}"
          :title "A name to identify yourself to THERMOS. Must be at least 5 characters long."
          :id :username
          :placeholder "your@email.com"}]]]
      [:div {:style (style :padding :0.5em :flex-grow 1)}
       [:label "Password: "
        [:input.text-input
         {:type :password :name :password :id :password
          :required :required
          :style (style :width :100%)
          :width :100%
          :title "A password, which you should remember. At least 6 characters."
          :minlength 6}]]]]

     (case (keyword flash)
       :check-mail
       [:div.card "A password recovery email has been sent. Check your email."]
       :exists
       [:div.card "That user already exists. If it is your email address, and you have forgotten your password, click recover."]
       :failed
       [:div.card "Login failed - your password or username are not correct."]
       nil)

     [:div.flex-cols
      [:input.button
       {:style (style :margin-left :auto)
        :type :submit :value "Login" :name :login}]
      [:input.button 
       {:type :submit :value "Recover" :name :forgot :style (style :margin-left :1em)
        :formnovalidate "formnovalidate"
        }]
      ]
     [:div
      [:p "If you do not have an account, you can sign up by filling in the fields above and clicking "
       [:input.button
        {:style (style :margin :0.1em)
         :type :submit :value "Create" :name :create}]"."
       ]
      [:p "You can give your email address as your username - if you do, we may use it for:"]
      [:ol
       [:li "Telling you about changes or maintenance to THERMOS"]
       [:li "Asking you about how you are using the application or why you are interested in it"]]
      [:p "If you do not use your email address, that's fine, but you won't be able to recover your password."]
      [:p "You can read more about how THERMOS uses your data and the terms under which this is offered "
       [:a {:href "/help/data-protection.html"} "here"] "."]
      
      
      ]]]
   
   [:footer.flex-cols {:style (style :margin-top :auto)}
    [:div {:style (style :width :10em :margin-right :1em)}
       [:img {:src "/img/european-flag.svg"
              :style (style :width :100% :height :auto)
              :alt "The EU flag."}]]
    [:div
     [:p {:style {:margin-top 0}} "THERMOS is a tool for designing heat networks."]
     [:p "For news and information, visit " [:a {:href "http://www.thermos-project.eu/"}
                                             "the project website."]]

     [:p "This project has received funding from the European Union’s Horizon 2020 research and innovation programme under grant agreement No 723636. Thanks EU!"]]]))



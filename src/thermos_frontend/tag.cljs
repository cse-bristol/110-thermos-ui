(ns thermos-frontend.tag
  (:require [reagent.core :as reagent]))

(defn component
  "A stateless component for display a little chunk of data in a small box like this -> [ 3 | Supply x ]
  It should be passed the following props:
    :body (string, required) The text to be displayed inside the tag;
    :count (string, optional) This will be displayed in a little sub-box on the left of the tag;
    :close (bool, optional) Whether or not to display a little x icon to close the tag;
    :on-close (function, optional) Function that gets called when you click on the close icon."
  [{key :key
    body :body
    count :count
    close :close
    on-select :on-select
    on-close :on-close
    class :class}]
  [:span {:key key :class (concat ["tag"] class)}
   (if count
     [:span.tag__counter count])
   [:span.tag__body {:on-click on-select} body]
   (if close
     [:span.tag__close {:on-click (fn [] (on-close key))}
      "×"])])

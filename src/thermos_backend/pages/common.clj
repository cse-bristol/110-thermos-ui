(ns thermos-backend.pages.common
  (:require [hiccup2.core :refer [html]]
            [hiccup.page :refer [include-js include-css]]
            [thermos-backend.current-uri :refer [*current-uri*]]))

(def source-sans-pro
  "https://fonts.googleapis.com/css?family=Source+Sans+Pro:400,400i,600,600i,700,700i")

(defmacro page [atts & body]
  `(.toString
    (html
     [:head
      [:title (str "THERMOS: " ~(:title atts))]
      (when *current-uri*
        [:base {:href (str *current-uri* "/")}])
      [:meta {:charset "UTF-8"}]
      [:meta {:name :viewport :content "width=device-width, initial-scale=1"}]
      (include-css "/css/common.css" source-sans-pro)]
     [:body.flex-rows
      [:header [:h1 "THERMOS - " ~(:title atts)]]
      [:div.flex-grow
       {:style {:margin "1em"}}
       ~@body]
      [:footer
       "Some footer stuff"]
      ~@(for [js-file (:js atts)] (include-js js-file))
      ])))


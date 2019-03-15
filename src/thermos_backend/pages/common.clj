(ns thermos-backend.pages.common
  (:require [hiccup2.core :refer [html]]
            [hiccup.util :refer [raw-string]]
            [hiccup.page :refer [include-js include-css]]
            [ring.util.anti-forgery :as anti-forgery]
            [ring.middleware.anti-forgery :refer [*anti-forgery-token*]]
            [thermos-backend.current-uri :refer [*current-uri*]]
            [thermos-pages.common :refer [style]]
            [clojure.data.json :as json]
            [clojure.string :as string]))

(def source-sans-pro
  "https://fonts.googleapis.com/css?family=Source+Sans+Pro:400,400i,600,600i,700,700i")

(defn preloaded-values [values]
  [:script {:type "text/javascript"}
   (raw-string
    (str "var thermos_preloads = "
         (json/write-str values)
         ";\n"))])

(defmacro page [atts & body]
  `(str
    (html
     [:head
      [:title (str "THERMOS: " ~(:title atts))]
      (when (and *current-uri*
                 (not (.endsWith *current-uri* "/")))
        [:base {:href (str *current-uri* "/")}])
      [:meta {:charset "UTF-8"}]
      [:meta {:name :viewport :content "width=device-width, initial-scale=1"}]
      (let [aft# (if (bound? #'*anti-forgery-token*)
                   (force *anti-forgery-token*)
                   false)]
        [:script {:type "text/javascript"}
         (raw-string
          (str "var ring_anti_forgery = " (json/write-str aft#) ";\n"))])
      
      (include-css "/css/common.css" source-sans-pro
                   ~@(:css atts))
      ]
     [:body.flex-rows
      [:header.flex-cols {:style (style :flex-shrink 0 :flex-grow 0)}
       
       [:h1  "THERMOS - " ~(:title atts)]
       [:span.menu {:style (style :margin-left :auto)}
        [:button {:style (style :vertical-align :middle)}
         [:img {:style (style :vertical-align :middle)
                :src "/favicon.ico" :width "16"}]
         [:span {:style (style :color "#ddd")} " ▼"]]
        
        [:div
         [:div [:a {:href "/"} "Home"]]
         [:div [:a {:href "/settings"} "Settings"]]
         [:div "Help"]
         [:div [:a {:href "/logout"} "Logout"]]]]
       ]
      [:div#page-body.flex-grow
       {:style {:margin "1em"}}
       ~@body]
      ;; [:footer
      ;;  "Some footer stuff"]

      ~@(for [js-file (:js atts)] `(include-js ~js-file))
      ])))

(defn anti-forgery-field
  "`ring.util.anti-forgery` but compatible with hiccup2"
  []
  (raw-string (anti-forgery/anti-forgery-field)))

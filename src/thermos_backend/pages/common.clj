(ns thermos-backend.pages.common
  (:require [hiccup2.core :refer [html]]
            [hiccup.util :refer [raw-string]]
            [hiccup.page :refer [include-js include-css]]
            [ring.util.anti-forgery :as anti-forgery]
            [ring.middleware.anti-forgery :refer [*anti-forgery-token*]]
            [thermos-backend.current-uri :refer [*current-uri*]]
            [thermos-pages.common :refer [style]]
            [thermos-pages.menu :refer [menu]]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [rum.core :as rum]))

(def source-sans-pro
  "https://fonts.googleapis.com/css?family=Source+Sans+Pro:400,400i,600,600i,700,700i")

(defn preloaded-values [values]
  [:script {:type "text/javascript"}
   (raw-string
    (str "var thermos_preloads = "
         (json/write-str (pr-str values))
         ";\n"))])

(defmacro page [{:keys [title body-style css js preload]
                 :or {body-style {:margin "1em"}}}
                & body]
  `(str
    (html
     [:head
      [:title (str "THERMOS: " ~title)]
      (when (and *current-uri*
                 (not (.endsWith *current-uri* "/")))
        [:base {:href (str *current-uri* "/")}])
      [:meta {:charset "UTF-8"}]
      [:meta {:name :viewport :content "width=device-width, initial-scale=1"}]
      (when-let [preload# ~preload] (preloaded-values preload#))
      (let [aft# (if (bound? #'*anti-forgery-token*)
                   (force *anti-forgery-token*)
                   false)]
        [:script {:type "text/javascript"}
         (raw-string
          (str "var ring_anti_forgery = " (json/write-str aft#) ";\n"))])
      
      (include-css "/css/common.css" source-sans-pro
                   ~@css)
      ]
     [:body.flex-rows
      [:header.flex-cols {:style (style :flex-shrink 0 :flex-grow 0)}
       [:h1  "THERMOS - " ~title]
       [:span {:style (style :margin-left :auto)}
        [:a {:style (style :margin-left :1em) :href "/"}
         [:img {:style {:vertical-align :middle}
                :src "/favicon.ico" :width "16"}]
         "Home"]
        [:a {:style (style :margin-left :1em)
             :href "/settings"} "Settings"]
        [:a {:style (style :margin-left :1em)
             :href "/help"} "Help"]
        [:a {:style (style :margin-left :1em)
             :href "/logout"} "Logout"]
        ]]
      
      [:div#page-body.flex-grow
       {:style ~body-style}
       ~@body]
      ;; [:footer
      ;;  "Some footer stuff"]

      ~@(for [js-file js] `(include-js ~js-file))
      ])))

(defn anti-forgery-field
  "`ring.util.anti-forgery` but compatible with hiccup2"
  []
  (raw-string (anti-forgery/anti-forgery-field)))

(defn prerender-rum [component]
  (raw-string (rum/render-html component)))

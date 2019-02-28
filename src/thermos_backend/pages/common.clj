(ns thermos-backend.pages.common
  (:require [hiccup2.core :refer [html]]
            [hiccup.util :refer [raw-string]]
            [hiccup.page :refer [include-js include-css]]
            [ring.util.anti-forgery :as anti-forgery]
            [thermos-backend.current-uri :refer [*current-uri*]]
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

(defn style
  "Convert a map into an inline stylesheet"
  [& {:keys [] :as m}]
  {:test #(assert (and
                   (= "display:flex;"
                      (style :display :flex))
                   (= "display:flex;position:absolute;"
                      (style :display :flex :position :absolute))
                   (= "width:100%;"
                      (style :width :100%))))}
  (when (seq m)
    (str
     (string/join
      ";"
      (for [[k v] m]
        (str
         (if (keyword k) (name k) (str k)) ":"
         (if (keyword v) (name v) (str v)))))
     ";")))

(defmacro page [atts & body]
  `(str
    (html
     [:head
      [:title (str "THERMOS: " ~(:title atts))]
      (when (and *current-uri*
                 (not= *current-uri* "/"))
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
      ~@(for [js-file (:js atts)] `(include-js ~js-file))
      ])))

(defn anti-forgery-field
  "`ring.util.anti-forgery` but compatible with hiccup2"
  []
  (raw-string (anti-forgery/anti-forgery-field)))

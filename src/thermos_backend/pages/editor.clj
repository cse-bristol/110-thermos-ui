(ns thermos-backend.pages.editor
  (:require [clojure.data.json :as json]
            [hiccup2.core :refer [html]]
            [hiccup.util :refer [raw-string]]
            [hiccup.page :refer [include-js include-css]]
            [ring.util.anti-forgery :as anti-forgery]
            [thermos-pages.spinner :as spinner]
            [thermos-pages.common :refer [style]]
            [thermos-backend.pages.common :refer [source-sans-pro preloaded-values]]))

(defn editor-page [name
                   initial-content
                   initial-mode
                   map-bounds]
  (str
   (html
    [:head
     (preloaded-values
      {:initial-state initial-content
       :name name
       :map-bounds map-bounds
       :mode initial-mode})
     [:title (str "THERMOS - " (or name "New network"))]
     [:meta {:charset "UTF-8"}]
     [:meta {:name :viewport :content "width=device-width, initial-scale=1"}]
     (include-css "/css/editor.css" source-sans-pro)]
    
   [:body
    [:div#app {:style (style :background :white)}
     [:div
      {:style (style :position :absolute
                     :top :50% :left :50%
                     :transform "translate(-50%, -50%)"
                     )}
      (spinner/spinner {:size 128})]
     ]
    (include-js "/js/editor.js")])))


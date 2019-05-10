(ns thermos-backend.pages.help
  (:require [hiccup.util :refer [raw-string]]
            [thermos-backend.pages.common :refer [page]]
            [thermos-pages.common :refer [style]]
            [clojure.java.io :as io]
            [resauce.core :as resauce]
            [net.cgrand.enlive-html :as enlive]

            [clojure.zip :as zip]
            [clojure.string :as string]))

;; (defn help-page []
;;   (page
;;    {:title "Help"
;;     :js ["/js/help.js"]
;;     :css ["/css/help.css"]
;;     :body-style {:display "flex"}}
;;    [:div#help-app.flex-grow
;;     [:div#help-menu-panel]
;;     [:div#help-content-panel]]))


(defn help-page [section]
  (page
   {:title      "Help"
    :set-base   false
    :js         ["https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_HTML"]
    :css        ["/css/help.css"]
    :body-style {:overflow-y :scroll
                 :display :flex
                 :flex-direction :column
                 :flex-grow 1
                 :flex-shrink 1}}
   ;; it would be neat to be able to take out the TOC and put it into its own box
   (raw-string (slurp (io/resource (str "help/" section))))))

(defn help-search [query]
  (page
   {:title (str "Help search for " query)}

   (let [help-pages (resauce/resource-dir "help")]
     (map
      (fn [u]
        (let [content
              (->> (enlive/html-resource u)
                   (first)
                   (zip/xml-zip)
                   (iterate zip/next)
                   (take-while (complement zip/end?))
                   (keep (fn [pos]
                           (if (not (zip/branch? pos))
                             (zip/node pos)

                             (let [{{href :href
                                     id :id} :attrs} (zip/node pos)]
                               (when (and id (.startsWith id "org"))
                                 {:a id})
                               ))))
                   (cons {:a "#"})
                   (partition-by string?)
                   (partition 2))

              content
              (for [c content]
                (let [id (:a (first (first c)))
                      text (apply str (second c))
                      text (.replaceAll text "\\s+" " ")
                      s-text (.toLowerCase
                              (.replaceAll
                               text
                               "[^a-zA-Z0-9.]+"
                               " "
                               )
                              )
                      words (string/split s-text #" +")
                      ]
                  [id text words]))

              id-to-content
              (into {}
                    (for [c content]
                      [(first c) (second c)]))

              word-to-id
              (group-by first
                        (for [c content
                              w (nth c 2)]
                          [w (nth c 0)]))
              
              ]
          word-to-id))
      help-pages))
   ))


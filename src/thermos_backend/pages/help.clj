;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.pages.help
  (:require [hiccup.util :refer [raw-string]]
            [thermos-backend.pages.common :refer [page]]
            [thermos-pages.common :refer [style]]
            [clojure.java.io :as io]
            [resauce.core :as resauce]
            
            [clojure.zip :as zip]
            [clojure.string :as string]

            [clucie.core :as clucie]
            [clucie.query :as query]
            [clucie.analysis :as analysis]
            [clucie.store :as store]

            [net.cgrand.enlive-html :as en]
            [clojure.tools.logging :as log]
            [thermos-backend.changelog :refer [changelog]]
            )
  (:import [org.apache.lucene.search.highlight
            Formatter
            Fragmenter
            Highlighter
            QueryScorer
            SimpleHTMLFormatter
            SimpleSpanFragmenter
            TokenSources]))

(defn help-page [section]
  (let [content (-> (str "help/" section)
                    (io/resource)
                    (en/html-resource))
        title (-> content
                  (en/select [:head :title])
                  (first)
                  (en/text))]
    (page
     {:title      (str "Help: " title)
      :set-base   false
      :js         ["https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_HTML"]
      :css        ["/css/help.css"]
      :body-style {:margin 0 :padding 0}
      }
     (let [toc (-> content
                   (en/select [:div#table-of-contents]))
           body (-> content
                    (en/select [:div#content])
                    (en/transform [:h1.title] nil)
                    (en/transform [:div#table-of-contents] nil))
           ]
       [:div {:style (style
                      :flex "1 1 auto"
                      :display :flex
                      :flex-direction :row
                      :overflow :hidden
                      :max-height :100%
                      )}
        (when (seq toc)
          [:div {:style (style
                         :flex "0 0 300px"
                         )}
           (raw-string (apply str (en/emit* toc)))])
        [:div {:style (style
                       :flex "1 1 auto"
                       :overflow :auto
                       )}
         (raw-string (apply str (en/emit* body)))]]))))


(defn- extract-headings
  "Given a url or resource which has some html in it, split that resource up into bits.
  Each bit is a map which contains :content, :path, :title, and :id"
  [url]

  (log/info "Extracting headings from" (.getPath url) "for help index")
  (try (with-open [stream (io/input-stream url)]

         (let [x (-> stream
                     (en/html-resource)
                     (en/transform [:div#table-of-contents] nil)
                     (en/transform [:table] nil))

               doc-title (-> x
                             (en/select [:h1.title])
                             (en/texts)
                             (->> (apply str)))
               
               x (en/select x [[:div#content]])
               
               path (.replaceAll (str url) "^.+/help/" "")
               z (zip/xml-zip (first x))
               heading #{:h1 :h2 :h3 :h4}
               flat
               (loop [z z
                      e []]
                 (cond
                   (zip/end? z)
                   e

                   (and (zip/branch? z)
                        (heading (:tag (zip/node z)))
                        (:id (:attrs (zip/node z))))
                   (recur (zip/right z)
                          (conj e (zip/node z)))

                   (zip/branch? z)
                   (recur (zip/next z) e)

                   :else
                   (recur (zip/next z) (conj e (zip/node z)))))
               flat (if (:tag (first flat)) flat (cons {:tag :h1} flat))
               flat (partition-by string? flat)
               flat (partition 2 flat) 
               ]
           (for [[heading content] flat]
             (let [best-heading (last (filter (comp :id :attrs) heading))
                   id (:id (:attrs best-heading))
                   title (apply str  (en/texts [best-heading]))
                   content (.replaceAll (apply str content) "\\s+" " ")
                   ]
               {:path path
                :id     id
                :doc-title doc-title
                :title   title
                :content content
                }))))
       (catch Exception e
         (log/error e "Error loading" url "for help index"))))

(def analyzer (analysis/standard-analyzer))
(def index (let [index (store/memory-store)
                 headings (->> (resauce/resource-dir "help")
                               (concat (resauce/resource-dir "help/network"))
                               (concat (resauce/resource-dir "help/supply"))
                               (remove #(let [name (.getFile %)]
                                          (or (= "index.html" name)
                                              (not (.endsWith name "html")))))
                               (mapcat extract-headings))
                 ]
             
             (clucie/add!
              index
              headings
              [:content :title]
              analyzer)
             
             index))

(defn help-search [query]
  (let [matches (into
                 (set (clucie/phrase-search index {:title query} 100))
                 (clucie/phrase-search index {:content query} 100))
        
        nmatches (count matches)
        matches (group-by (juxt :path :doc-title) matches)]
    
    (page
     {:js         ["https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_HTML"]
      :title (str "Help search for " query " - " nmatches " matches")
      :css        ["/css/help.css"]
      :body-style {:overflow :auto}
      }

     [:form {}
      [:input {:type :text :name :q :value query}]
      [:input {:type :submit :value :Search}]]
     
     (if (empty? matches)
       [:div "No results found"]
       
       (let [formatter (SimpleHTMLFormatter.)
             scorer (QueryScorer. (query/parse-form {:content query}))
             highlighter (Highlighter. formatter scorer)
             fragmenter (SimpleSpanFragmenter. scorer 10)
             fragment-content
             (fn [match]
               (let [stream (TokenSources/getAnyTokenStream
                             (store/store-reader index)
                             (:doc-id (meta match))
                             "content"
                             analyzer)
                     fragments (.getBestFragments highlighter stream (:content match) 20)]
                 [:div
                  (interpose " ... "
                             (for [fragment fragments]
                               [:span (raw-string fragment)]))]))
             ]
         
         (.setTextFragmenter highlighter fragmenter)

         [:ul
          (for [[[path doc-title] submatches] matches]
            (let [{headings false [toplevel] true}
                  (group-by #(= "" (:id %)) submatches)]
              [:li
               [:h3 "In " [:a {:href (str "/help/" path)} doc-title] ":"]
               (when toplevel
                 (fragment-content toplevel))
               
               (when (seq headings)
                 [:ul
                  (for [match (sort-by :title headings)]
                    [:li [:a {:href (str "/help/" (:path match) "#" (:id match))} (:title match)]
                     (fragment-content match)])])])
            
            )
          ])))))

(defn help-changelog []
  (page
   {:title "Recent changes"
    :css   ["/css/help.css"]
    :body-style {:margin 0 :padding 0 :display :flex :flex-direction :column}}
   [:div#content {:style (style :overflow :auto :max-width :100%)}
    (for [release changelog]
      [:div
       [:h2 (:title release)]
       [:dl (apply concat
                   (for [change (:changes release)]
                     [[:dt (:title change)]
                      [:dd (:summary change)]]))]]
      
      )]))


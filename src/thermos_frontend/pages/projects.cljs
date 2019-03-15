(ns thermos-frontend.pages.projects
  (:require [reagent.core :as reagent]
            [cljsjs.react]
            [thermos-pages.projects :as template]
            [goog.net.XhrIo :as xhr]
            [cognitect.transit :as transit]))

(enable-console-print!)

(defonce state (reagent/atom nil))
(defonce mounted (atom false))
(def reader (transit/reader :json))

(defn- render []
  (println "render!!!")
  [template/project-page-body @state])

(defn- mount []
  (reset! mounted true)
  (js/ReactDOM.hydrate
   (reagent/as-element [render])
   (js/document.getElementById "page-body")))

(defn- poll-project-status []
  (xhr/send
   "poll.t"
   (fn [response]
     (reset! state
             (transit/read reader (.. response -target getResponseText)))
     (when-not @mounted (mount)))))

(defonce poll-timer
  (js/window.setInterval poll-project-status 3000))

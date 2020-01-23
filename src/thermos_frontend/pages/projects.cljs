(ns thermos-frontend.pages.projects
  (:require [rum.core :as rum]
            [cljsjs.react]
            [thermos-pages.project-components :as template]
            [goog.net.XhrIo :as xhr]
            [cognitect.transit :as transit]))

(enable-console-print!)

(defonce state (atom nil))
(defonce mounted (atom false))
(def reader (transit/reader :json))

(declare poll-project-status)

;; TODO here, re-render immediately after someone's clicked a button
;; to remove the expected 1.5s delay.
(rum/defc render < rum/reactive []
  (template/project-page-body
   (rum/react state)
   :on-event poll-project-status))

(defn- mount []
  (reset! mounted true)
  (rum/hydrate
   (render state)
   (js/document.getElementById "page-body")))

(defn- poll-project-status []
  (xhr/send
   "poll.t"
   (fn [response]
     (reset! state
             (transit/read reader (.. response -target getResponseText)))
     (when-not @mounted (mount)))))


(poll-project-status)

(defonce poll-timer
  (js/window.setInterval poll-project-status 5000))

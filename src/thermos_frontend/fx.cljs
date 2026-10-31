;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.fx
  "re-frame effects for the editor: talking to the server, and other
  things that touch the browser. Effects which get a response take the
  event to dispatch with it as :on-response, and conj the response onto it."
  (:require [re-frame.core :as rf]
            [goog.net.XhrIo :as xhr]
            [cognitect.transit :as transit]
            [cljs.reader :as reader]))

(defn- response-headers
  "What the server says about the network and its run, in the headers of
  a response to saving or loading it"
  [^js xhr]
  {:run-state      (when-let [s (.getResponseHeader xhr "x-run-state")]
                     (keyword (.substring s 1)))
   :queue-position (when-let [s (.getResponseHeader xhr "x-queue-position")]
                     (js/parseInt s))})

(rf/reg-fx
 ::save
 (fn [{:keys [title run content on-response]}]
   (xhr/send
    (if run (str "?run=" (name run)) "")
    (fn [e]
      (let [^js target (.-target e)]
        (rf/dispatch
         (conj on-response
               (assoc (response-headers target)
                      :status   (.getStatus target)
                      :location (.getResponseHeader target "Location"))))))
    "POST"
    (let [data (js/FormData.)
          blob (js/Blob. #js [content] #js {"type" "application/edn"})]
      (.append data "name" title)
      (.append data "content" blob "content.edn")
      data))))

(rf/reg-fx
 ::load
 (fn [{:keys [on-response]}]
   (xhr/send
    ""
    (fn [e]
      (let [^js target (.-target e)]
        (when (.isSuccess target)
          (rf/dispatch
           (conj on-response
                 (assoc (response-headers target)
                        :document (reader/read-string (.getResponseText target))))))))
    "GET"
    nil
    #js {"Accept" "application/edn"})))

;; the server sends the run status as transit
(rf/reg-fx
 ::run-status
 (fn [{:keys [on-response]}]
   (xhr/send
    (str js/window.location.pathname "/status")
    (fn [e]
      (let [^js target (.-target e)]
        (when (.isSuccess target)
          (rf/dispatch
           (conj on-response
                 (transit/read (transit/reader :json) (.getResponseText target)))))))
    "GET")))

(defonce ^:private poll-timer (atom nil))

;; dispatch the given event every second, or stop if it is nil
(rf/reg-fx
 ::poll
 (fn [event]
   (swap! poll-timer
          (fn [timer]
            (cond
              (and timer event) timer
              event             (js/window.setInterval #(rf/dispatch event) 1000)
              :else             (do (when timer (js/window.clearInterval timer)) nil))))))

(rf/reg-fx
 ::replace-url
 (fn [url]
   (when (and url (not= (.-pathname js/window.location) url))
     (js/window.history.replaceState nil "Editor" url))))

(rf/reg-fx
 ::call
 (fn [[f & args]]
   (apply f args)))

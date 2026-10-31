;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.popover
  "The editor's popover: one menu or dialog at a time, shown at a screen
  position or in the middle. What's showing is in app-db under ::popover,
  which document/keep-interesting strips off."
  (:require [re-frame.core :as rf]))

;; the element to give the focus back to when the popover closes
(defonce focus-element (atom nil))

(rf/reg-event-db
 ::open
 (fn [db [_ content coords]]
   (assoc db ::popover {:content content :coords coords})))

(rf/reg-event-fx
 ::close
 (fn [{db :db} _]
   {:db (dissoc db ::popover)
    ::restore-focus nil}))

(rf/reg-fx
 ::restore-focus
 (fn [_]
   (when-let [e @focus-element]
     (.focus e))))

(rf/reg-sub ::popover (fn [db _] (::popover db)))

(defn component
  []
  (when-let [{:keys [content coords]} @(rf/subscribe [::popover])]
    (cond
      (seqable? coords)
      [:div {:style {:position :fixed
                     :top (str (second coords) "px")
                     :left (str (first coords) "px")
                     :z-index "2000"
                     :display :block}}
       content]

      (= :middle coords)
      [:div {:style {:position :fixed
                     :top 0
                     :left 0
                     :width :100%
                     :height :100%
                     :z-index "2000"
                     :display :flex
                     }}
       [:div {:style {:margin :auto}} content]])))

(defn open!
  "Show `content` at `coords`, which is [x y] on the screen or :middle"
  [content coords]
  (rf/dispatch [::open content coords]))

(defn close!
  "Close the popover. Takes and ignores any arguments, so it can be an
  event handler."
  [& _]
  (rf/dispatch [::close]))

(defn set-focus-element! [e]
  (reset! focus-element e))

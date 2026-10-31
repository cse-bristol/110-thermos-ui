;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.toaster
  "Brief messages at the bottom of the editor. The current one is in
  app-db under ::toast, which document/keep-interesting strips off."
  (:require [re-frame.core :as rf]))

(rf/reg-event-fx
 ::show
 (fn [{db :db} [_ content duration-ms]]
   (let [expires (+ (js/Date.now) duration-ms)]
     {:db (assoc db ::toast {:showing true :content content :expires expires})
      :fx [[:dispatch-later {:ms duration-ms :dispatch [::expire]}]]})))

(rf/reg-event-db
 ::expire
 ;; hide the toast, unless a later one has replaced it
 (fn [db _]
   (cond-> db
     (>= (js/Date.now) (get-in db [::toast :expires] 0))
     (assoc-in [::toast :showing] false))))

(rf/reg-event-db
 ::hide
 (fn [db _]
   (update db ::toast assoc :showing false :expires 0)))

(rf/reg-sub ::toast (fn [db _] (::toast db)))

(defn show!
  "Show `content` for `duration` seconds"
  [content & {:keys [duration]
              :or {duration 2}}]
  (rf/dispatch [::show content (* 1000 duration)]))

(defn hide! []
  (rf/dispatch [::hide]))

(defn component
  []
  (let [{showing :showing content :content} @(rf/subscribe [::toast])]
    [(if showing
       :div.toaster__container.toaster__container--showing
       :div.toaster__container)
     (when showing content)
     ]))

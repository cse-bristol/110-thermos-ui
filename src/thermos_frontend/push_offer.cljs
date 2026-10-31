;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.push-offer
  "The first time a run may take a while, offer to send a notification in
  this browser when it's done, unless the browser has them already or
  can't have them. Push subscriptions themselves are handled by
  resources/public/js/push.js, which is window.thermosPush.

  The offer's state is in app-db under ::offer, which
  document/keep-interesting strips off: nil until there's a long run, then
  :checking, :show, :enabled or :hide."
  (:require [re-frame.core :as rf]
            [goog.object :as gobj]))

(def ^:private offer-seconds
  "Offer push notifications for runs that may take longer than this"
  (* 20 60))

(def ^:private offered-key
  "The localStorage key which says the offer has been made in this browser"
  "thermos-push-offered")

(defn- push-api [] (gobj/get js/window "thermosPush"))

;; Whether push notifications are possible here, and have been offered before
(rf/reg-cofx
 ::browser
 (fn [cofx _]
   (assoc cofx ::browser
          {:push-available (some? (push-api))
           :offered-before (try (some? (.getItem js/localStorage offered-key))
                                (catch :default _ true))})))

;; Call `method` of window.thermosPush, and dispatch `on-result` with its result
;; or `on-error` if it fails
(rf/reg-fx
 ::push-api
 (fn [{:keys [method on-result on-error]}]
   (-> (js-invoke (push-api) method)
       (.then #(rf/dispatch (conj on-result %)))
       (.catch #(rf/dispatch on-error)))))

(rf/reg-fx
 ::remember-offered
 (fn [_]
   (try (.setItem js/localStorage offered-key "yes")
        (catch :default _))))

(rf/reg-event-fx
 :push-offer/consider
 ;; given the timing of a run which is queued or running
 [(rf/inject-cofx ::browser)]
 (fn [{db :db {:keys [push-available offered-before]} ::browser}
      [_ {:keys [finishes-in]}]]
   (when (and (nil? (::offer db))
              (number? finishes-in)
              (> finishes-in offer-seconds))
     (if (and push-available (not offered-before))
       {:db (assoc db ::offer :checking)
        ::push-api {:method "state"
                    :on-result [::state-checked]
                    :on-error [::hide]}}
       {:db (assoc db ::offer :hide)}))))

(rf/reg-event-fx
 ::state-checked
 (fn [{db :db} [_ state]]
   (if (= state "off")
     {:db (assoc db ::offer :show)
      ::remember-offered nil}
     {:db (assoc db ::offer :hide)})))

(rf/reg-event-fx
 ::enable
 (fn [_ _]
   {::push-api {:method "enable"
                :on-result [::enabled]
                :on-error [::hide]}}))

(rf/reg-event-db
 ::enabled
 (fn [db [_ state]]
   (assoc db ::offer (if (= state "on") :enabled :hide))))

(rf/reg-event-db
 ::hide
 (fn [db _]
   (assoc db ::offer :hide)))

(rf/reg-sub ::offer (fn [db _] (::offer db)))

(defn component []
  (case @(rf/subscribe [::offer])
    :show
    [:div {:style {:margin "0.5em 0" :padding "0.5em" :background "#eef6fb"
                   :display :flex :align-items :center :gap "0.5em"}}
     [:span {:style {:flex-grow 1}}
      "This could take a while. Would you like a notification in this browser when it's done?"]
     [:button.button {:on-click #(rf/dispatch [::enable])} "Notify me"]
     [:button.button {:on-click #(rf/dispatch [::hide])} "No thanks"]]

    :enabled
    [:div {:style {:margin "0.5em 0"}}
     "We'll let you know when it's done. You can change this in your account settings."]

    nil))

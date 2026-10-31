;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.subs
  "re-frame subscriptions for the editor"
  (:require [re-frame.core :as rf]
            [thermos-frontend.db :as db]))

(rf/reg-sub
 :doc/get
 (fn [db [_ path]]
   (get-in db path)))

;; Saving and running

(rf/reg-sub
 :server
 (fn [db _]
   (db/server db)))

(rf/reg-sub
 :server/needs-save
 :<- [:server]
 (fn [server _] (:needs-save server)))

(rf/reg-sub
 :server/running-or-queued
 :<- [:server]
 (fn [server _] (contains? db/running-states (:run-state server))))

(rf/reg-sub
 :server/run
 :<- [:server]
 (fn [server _]
   (select-keys server [:run-state :queue-position :run-message :run-timing])))

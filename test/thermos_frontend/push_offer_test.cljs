;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.push-offer-test
  (:require [cljs.test :refer [deftest testing is use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db]
            [thermos-frontend.push-offer :as push-offer]))

(use-fixtures :each
  {:before #(set! (.-restore js/goog.global) (rf/make-restore-fn))
   :after  #((.-restore js/goog.global))})

(def long-run {:finishes-in (* 60 60)})

(defn- offer-after
  "The offer state after these events, in a browser like `browser`, where
  window.thermosPush answers `push-results`, a map of method to result"
  [browser push-results & events]
  (let [replies (atom [])]
    (rf/reg-cofx ::push-offer/browser #(assoc % ::push-offer/browser browser))
    ;; push.js answers later, so queue its replies and dispatch them after
    (rf/reg-fx ::push-offer/push-api
               (fn [{:keys [method on-result]}]
                 (swap! replies conj (conj on-result (push-results method)))))
    (rf/reg-fx ::push-offer/remember-offered (fn [_]))
    (reset! re-frame.db/app-db {})
    (doseq [e events]
      (rf/dispatch-sync e)
      (while (seq @replies)
        (let [reply (first @replies)]
          (swap! replies (comp vec rest))
          (rf/dispatch-sync reply))))
    (::push-offer/offer @re-frame.db/app-db)))

(def new-browser {:push-available true :offered-before false})

(deftest consider
  (is (nil? (offer-after new-browser {} [:push-offer/consider {:finishes-in 60}]))
      "a short run")
  (is (nil? (offer-after new-browser {} [:push-offer/consider nil]))
      "no timing yet")
  (is (= :show (offer-after new-browser {"state" "off"} [:push-offer/consider long-run])))
  (is (= :hide (offer-after new-browser {"state" "on"} [:push-offer/consider long-run]))
      "notifications are already on")
  (is (= :hide (offer-after (assoc new-browser :offered-before true) {}
                            [:push-offer/consider long-run])))
  (is (= :hide (offer-after (assoc new-browser :push-available false) {}
                            [:push-offer/consider long-run])))
  (testing "only once"
    (is (= :hide (offer-after new-browser {"state" "off"}
                              [:push-offer/consider long-run]
                              [::push-offer/hide]
                              [:push-offer/consider long-run])))))

(deftest answering
  (is (= :enabled (offer-after new-browser {"state" "off" "enable" "on"}
                               [:push-offer/consider long-run]
                               [::push-offer/enable])))
  (is (= :hide (offer-after new-browser {"state" "off" "enable" "denied"}
                            [:push-offer/consider long-run]
                            [::push-offer/enable]))
      "the browser didn't allow it"))

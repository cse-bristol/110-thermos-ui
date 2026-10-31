;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.preload
  "In thermos-backend.pages.common there's code to stick js variables into
  scope at the top of the page.

  This is the other half of that, providing access to them. The
  purpose is to pass some info into a cljs page without having to load
  it and then fire off an xmlhttprequest."
  (:require [cljs.reader :refer [read-string]]))

;; goog.global rather than js/window, so this loads in tests on node,
;; where there are no preloads
(def preloaded-values
  (atom (some-> (aget js/goog.global "thermos_preloads")
                (read-string))))

(aset js/goog.global "thermos_preloads" nil)

(defn get-value [key & {:keys [clear]}]
  (let [out (get @preloaded-values key)]
    (when clear (swap! preloaded-values dissoc key))
    out))

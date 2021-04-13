;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.util
  (:require [ajax.core :refer [POST]]))

(defn target-value [e]
  (.. e -target -value))

(defn focus-next []
  (let [candidates (->> (js/document.querySelectorAll
                         "a:not([disabled]), button:not([disabled]), input[type=text]:not([disabled]), [tabindex]:not([disabled]):not([tabindex=\"-1\"])")
                        (js/Array.prototype.slice.call)
                        (filter
                         (fn [el]
                           (or (pos? (.-offsetWidth el))
                               (pos? (.-offsetHeight el))
                               (= js/document.activeElement el)))))

        target (->> (concat candidates [(first candidates)])
                    (drop-while #(not= js/document.activeElement %))
                    (second))
        ]
    (when target
      (.focus target)
      (.select target))))


(defn upload-file [url & {:keys [accept handler]}]
  (let [file (js/document.createElement "input")]
    (set! (.-type file) "file")
    (when accept (set! (.-accept file) accept))
    (set! (.-onchange file)
          (fn [e]
            (when-let [file (-> e .-target .-files (aget 0))]
              (POST url
                  {:body
                   (doto (js/FormData.)
                     (.append "file" file "input.xlsx"))
                   :handler handler
                   }))))
    (.click file)))

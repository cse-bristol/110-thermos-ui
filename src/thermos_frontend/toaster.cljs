;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.toaster
  (:require [reagent.core :as reagent]))

(defonce state (reagent/atom {:showing false
                              :content []}))

(defn show! [content & {:keys [ duration coordinates ]
                        :or { duration 2 }
                        }]
  (let [duration (* 1000 duration)]
    (swap! state
           assoc
           :showing true
           :content content
           :expires (+ (js/Date.now) duration))
    
    (js/window.setTimeout
     #(let [now (js/Date.now)]
        (swap! state
               (fn [{e :expires :as state}]
                 (if (>= now e)
                   (assoc state :showing false)
                   state))))
     duration)))

(defn hide! []
  (swap! state assoc :showing false :expires 0))

(defn component
  []
  (let [{showing :showing content :content} @state]
    [(if showing
       :div.toaster__container.toaster__container--showing
         :div.toaster__container)
     (when showing content)
     ]))






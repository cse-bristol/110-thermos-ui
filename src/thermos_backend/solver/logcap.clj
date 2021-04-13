;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.solver.logcap
  (:import [org.apache.log4j
            LogManager
            Appender
            SimpleLayout]
           [org.apache.log4j.spi Filter LoggingEvent])
  (:require [clojure.tools.logging :as log]
            [clojure.string :as string]))

(defn appender-glue ^Appender [on-message]
  (let [thread-id (.getId (Thread/currentThread))
        layout (SimpleLayout.)]
    
    (reify Appender
      (^void doAppend [this ^LoggingEvent event]
       ;; making an assumption here: logging does happen on same thread
       (when (= thread-id (.getId (Thread/currentThread)))
         (on-message (string/trim (.format layout event))))))))

(defmacro with-log-messages [on-message & body]
  `(let [appender# (appender-glue ~on-message)]
     (try
       (.addAppender (LogManager/getRootLogger) appender#)
       ~@body
       (finally (.removeAppender (LogManager/getRootLogger) appender#)))))

(comment
  (let [msgs (atom [])]
    (with-log-messages (fn [msg] (swap! msgs conj msg))
      (dotimes [i 1] (log/info "SOME WORDS" i)))
    (println "CApped: " (count @msgs)))
  )

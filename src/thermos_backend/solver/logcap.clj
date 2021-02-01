(ns thermos-backend.solver.logcap
  (:import [org.apache.log4j
            AppenderSkeleton
            LogManager
            Logger
            WriterAppender
            SimpleLayout]
           [org.apache.log4j.spi Filter LoggingEvent])
  (:require [clojure.tools.logging :as log]))

(defmacro with-log-into2 [writer & body]
  `(let [thread-name# (.getName (Thread/currentThread))
         appender# (WriterAppender. (SimpleLayout.) ~writer)
         filter# (proxy [Filter] []
                   (decide [^LoggingEvent event#]
                     (if (= thread-name# (.getThreadName event#))
                         Filter/ACCEPT Filter/DENY)))]
     (try
       (.addFilter appender# filter#)
       (.addAppender (LogManager/getRootLogger) appender#)
       ~@body
       (finally (.removeAppender (LogManager/getRootLogger) appender#)))))

(defmacro with-log-into [writer & body]
  `(let [appender# (WriterAppender. (SimpleLayout.) ~writer)]
     (try
       (.addAppender (LogManager/getRootLogger) appender#)
       ~@body
       (finally (.removeAppender (LogManager/getRootLogger) appender#)))))


(comment
  (let [w (java.io.StringWriter.)]
    (with-log-into2 w
      (dotimes [i 1] (log/info "SOME WORDS" i)))
    (println "CApped: "(.toString w))))

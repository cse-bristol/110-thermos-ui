(ns thermos-backend.solver.logcap
  (:import [org.apache.log4j
            AppenderSkeleton
            LogManager
            Logger
            WriterAppender
            SimpleLayout])
  (:require [clojure.tools.logging :as log]))

(defmacro with-log-into [writer & body]
  `(let [appender# (WriterAppender. (SimpleLayout.) ~writer)]
     (try
       (.addAppender (LogManager/getRootLogger) appender#)
       ~@body
       (finally (.removeAppender (LogManager/getRootLogger) appender#)))))

(comment
  (let [w (java.io.StringWriter.)]
    (with-log-into w
      (dotimes [i 1] (log/info "SOME WORDS" i)))
    (println "CApped: "(.toString w))))

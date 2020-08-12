(ns thermos-frontend.events.core
  (:require [thermos-frontend.operations :as operations]))

(defmulti handle
  (fn [_ event]
    (if
      (keyword? (first event))
      (first event)
      ::apply)))

(defmethod handle ::apply
  [state [f & args]]
  (apply f state args))

(defmethod handle :select-all
  [state _]
  (operations/select-all-candidates state))

(defmethod handle :select-ids
  [state [_ ids method]]
  (operations/select-candidates state ids (or method :replace)))


(defmethod handle :default
  [state e]
  (println "UNKNOWN UI EVENT:" e)
  state)

(load "pipe")

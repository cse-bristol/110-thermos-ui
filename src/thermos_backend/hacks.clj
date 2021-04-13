;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.hacks
  (:require [compojure.core]
            [ring.util.response]))

;; argh, neither of these things work if you AOT compile

;; this is a horrible way to apply a fix for a bug but is the easiest
;; option for now, because the patched version of compojure is not
;; released and can't be used as a git dependency

(in-ns 'compojure.core)

(defn ^:no-doc make-context [route make-handler]
  (letfn [(handler
            ([request]
             (when-let [context-handler (make-handler request)]
               (context-handler request)))
            ([request respond raise]
             (if-let [context-handler (make-handler request)]
               (context-handler request respond raise)
               (respond nil))))]
    (if (#{":__path-info" "/:__path-info"} (:source route))
      handler
      (fn
        ([request]
         (if-let [request (context-request request route)]
           (handler request)))
        ([request respond raise]
         (if-let [request (context-request request route)]
           (handler request respond raise)
           (respond nil)))))))

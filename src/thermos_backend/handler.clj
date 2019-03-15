(ns thermos-backend.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.middleware :refer [wrap-canonical-redirect]]

            [ring.middleware.defaults :refer [wrap-defaults site-defaults api-defaults]]

            [ring.logger :as logger]
            [clojure.tools.logging :as log]
            [thermos-backend.config :refer [config]]
            [mount.core :refer [defstate]]

            [ring.util.response :as response]
            [clojure.edn :as edn]

            [thermos-backend.pages.core :as pages]
            [thermos-backend.auth :as auth]
            [thermos-backend.current-uri :as current-uri]))

(defn wrap-no-cache [handler]
  (if (= "true" (config :web-server-disable-cache))
    (do (log/info "Disabling caching")
        (fn [request]
          (when-let [response (handler request)]
            (assoc-in response [:headers "Cache-Control"] "no-store"))))
    handler))

(defroutes site-routes
  auth/auth-routes
;;  data/data-routes
  pages/page-routes)

(defstate all
  :start
  (-> site-routes
      ;;(wrap-canonical-redirect)
      (auth/wrap-auth)
      (current-uri/wrap-current-uri)
      (wrap-defaults (-> site-defaults
                         (assoc-in [:security :anti-forgery] false)))
      (wrap-no-cache)
;      (wrap-canonical-redirect)
      ))

;; (in-ns 'compojure.middleware)


;; (defn wrap-canonical-redirect
;;   "Middleware that permanently redirects any non-canonical route to its
;;   canonical equivalent, based on a make-canonical function that changes a URI
;;   string into its canonical form. If not supplied, the make-canonical function
;;   will default to [[remove-trailing-slash]]."
;;   ([handler]
;;    (wrap-canonical-redirect handler remove-trailing-slash))
;;   ([handler make-canonical]
;;    (let [redirect-handler (wrap-routes handler (constantly redirect-to-canonical))]
;;      (fn
;;        ([{uri :uri :as request}]
;;         (println "maybe redirect")
;;         (let [canonical-uri (make-canonical uri)]
;;           (if (= uri canonical-uri)
;;             (do
;;               (println "dont redirect")
;;               (handler request))
;;             (do
;;               (println "do redirect")
;;               (redirect-handler (assoc-path request canonical-uri))))))
;;        ([{uri :uri :as request} respond raise]
;;         (let [canonical-uri (make-canonical uri)]
;;           (if (= uri canonical-uri)
;;             (do (println "no redir 2")
;;                 (handler request respond raise))
;;             (do (println "do redir 2")
;;                 (redirect-handler (assoc-path request canonical-uri) respond raise)))))))))


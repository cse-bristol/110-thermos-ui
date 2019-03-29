(ns thermos-backend.email
  "Stuff for emailing people about their user account & jobs"
  (:require [postal.core :as postal]
            [thermos-backend.config :refer [config]]
            [clojure.tools.logging :as log]
            [thermos-backend.queue :as queue]))

;; emails are processed on the queue - no idea if this is good

(defn- send-message [message]
  (let [smtp-config
        (cond-> {}
          (:smtp-host config)
          (assoc :host (:smtp-host config))
          (:smtp-port config)
          (assoc :port (Integer/parseInt (:smtp-port config)))
          (:smtp-user config)
          (assoc :user (:smtp-user config))
          (:smtp-password config)
          (assoc :pass (:smtp-password config))
          (:smtp-ssl config)
          (assoc :ssl (= "true" (:smtp-ssl config)))
          (:smtp-tls config)
          (assoc :tls (= "true" (:smtp-tls config))))
        
        message (assoc message :from (:smtp-from-address config))]
    
    (try (postal/send-message smtp-config message)
         (catch Exception e
           (log/error e "Unable to send message" config smtp-config message)))))

(defn- queue-message [message]
  (queue/enqueue :emails message))

(queue/consume :emails send-message)

(defn- format-token [token]
  (str (:base-url config) "/token/" token))

(defn send-password-reset-token [user token]
  (queue-message
   {:to user
    :subject "THERMOS password reset"
    :body (str
           "To reset your THERMOS password, visit:\n\n"
           (format-token token)
           "\n\n"
           "If you have not forgotten your password you can safely ignore this.")}))

(defn send-invitation-message
  "Send an invitation to join a project to a user.
  If `token` is not nil, it's assumed the account is newly created by
  the invitation, and the token will be included in the email so they
  can log in."
  [user invited-by project-name
   token]
  (queue-message
   {:to user
    :subject (str "Invitation to THERMOS project " project-name)
    :body
    (if token
      (str
       invited-by " has invited you to use THERMOS, a heat network modelling tool, "
       "to work on a project called " project-name "\n\n"
       "If you are interested, you can login by visiting:\n\n"
       (format-token token)
       "\n"
       "If not, you can safely ignore this message.")

      (str
       invited-by " has invited you to a THERMOS project called " project-name "."
       ))}))

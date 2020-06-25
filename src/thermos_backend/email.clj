(ns thermos-backend.email
  "Stuff for emailing people about their user account & jobs"
  (:require [postal.core :as postal]
            [thermos-backend.config :refer [config]]
            [clojure.tools.logging :as log]
            [thermos-backend.queue :as queue]
            [clojure.string :as string]))

;; emails are processed on the queue - no idea if this is good

(defn- send-message [message progress]
  (let [smtp-config
        (cond-> {}
          (config :smtp-host)
          (assoc :host (config :smtp-host))
          (config :smtp-port)
          (assoc :port (config :smtp-port))
          (:smtp-user config)
          (assoc :user (config :smtp-user))
          (:smtp-password config)
          (assoc :pass (config :smtp-password))
          (:smtp-ssl config)
          (assoc :ssl (config :smtp-ssl))
          (:smtp-tls config)
          (assoc :tls (config :smtp-tls)))
        
        message (assoc message :from (config :smtp-from-address))]
    
    (try (postal/send-message smtp-config message)
         (catch Exception e
           (log/error e "Unable to send message" config smtp-config message)))))

(defn- queue-message [message]
  (queue/enqueue :emails message))

(queue/consume :emails send-message)

(defn- format-token [token]
  (str (config :base-url) "/token/" token))

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

(defn send-system-message
  "Send a message to all system users except those who aren't interested"
  [users subject message]
  {:pre [(not (empty? users))
         (string? subject)
         (string? message)
         (not (string/blank? subject))
         (not (string/blank? message))
         (every? (comp string? :id) users)]}
  (queue-message
   {:bcc (map :id users)
    :subject (str "THERMOS: " subject)
    :body (format "%s
----
You are receiving this message because you have an account on THERMOS.
You change your settings at %s/settings to unsubscribe from any message like this."
                  message
                  (config :base-url))}))


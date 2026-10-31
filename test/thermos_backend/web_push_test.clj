;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.web-push-test
  (:require [thermos-backend.web-push :as web-push]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.test :as t])
  (:import [java.security KeyPair Signature]
           [java.util Base64]))

(defn- unb64 [s] (.decode (Base64/getUrlDecoder) ^String s))

;; the example from RFC 8291 appendix A
(def rfc-8291
  {:plaintext   "V2hlbiBJIGdyb3cgdXAsIEkgd2FudCB0byBiZSBhIHdhdGVybWVsb24"
   :as-public   "BP4z9KsN6nGRTbVYI_c7VJSPQTBtkgcy27mlmlMoZIIgDll6e3vCYLocInmYWAmS6TlzAC8wEqKK6PBru3jl7A8"
   :as-private  "yfWPiYE-n46HLnH0KqZOF1fJJU3MYrct3AELtAQ-oRw"
   :ua-public   "BCVxsr7N_eNgVRqvHtD0zTZsEc6-VV-JvLexhqUzORcxaOzi6-AYWXvTBHm4bjyPjs7Vd8pZGH6SRpkNtoIAiw4"
   :salt        "DGv6ra1nlYgDCS1FRnbzlw"
   :auth-secret "BTBZMqHH6r4Tts7J_aSIgg"
   ;; these are encoded separately in the RFC
   :header      (str "DGv6ra1nlYgDCS1FRnbzlwAAEABBBP4z9KsN6nGRTbVYI_c7VJSPQTBtkgcy27ml"
                     "mlMoZIIgDll6e3vCYLocInmYWAmS6TlzAC8wEqKK6PBru3jl7A8")
   :ciphertext  (str "8pfeW0KbunFT06SuDKoJH9Ql87S1QUrdirN6GcG7sFz1y1sqLgVi1VhjVkHsUoEs"
                     "bI_0LpXMuGvnzQ")})

(t/deftest rfc-8291-example
  (let [{:keys [plaintext as-public as-private ua-public salt auth-secret header ciphertext]} rfc-8291
        key-pair (KeyPair. (#'web-push/->public-key (unb64 as-public))
                           (#'web-push/->private-key (unb64 as-private)))]
    (t/is (= (concat (unb64 header) (unb64 ciphertext))
             (seq (web-push/encrypt (unb64 plaintext) ua-public auth-secret
                                    :salt (unb64 salt)
                                    :local-key-pair key-pair))))))

(t/deftest vapid-header
  (let [keys (web-push/generate-keys)
        header (web-push/vapid-header "https://push.example.net/some/path?x=1"
                                      keys "mailto:someone@example.com")
        [_ token k] (re-matches #"vapid t=([^,]+), k=(.+)" header)
        [h c s] (string/split token #"\.")
        claims (json/read-str (String. (unb64 c) "UTF-8"))]
    (t/is (= (:public-key keys) k))
    (t/is (= {"typ" "JWT" "alg" "ES256"} (json/read-str (String. (unb64 h) "UTF-8"))))
    (t/is (= "https://push.example.net" (claims "aud")))
    (t/is (= "mailto:someone@example.com" (claims "sub")))
    (t/is (< (quot (System/currentTimeMillis) 1000) (claims "exp")))
    (t/is (.verify (doto (Signature/getInstance "SHA256withECDSAinP1363Format")
                     (.initVerify (#'web-push/->public-key (unb64 k)))
                     (.update (.getBytes (str h "." c) "UTF-8")))
                   (unb64 s)))))

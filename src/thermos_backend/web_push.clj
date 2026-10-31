;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-backend.web-push
  "Sending Web Push messages to browsers (RFC 8030), with the payload
  encrypted as RFC 8291 says, and identifying ourselves with VAPID (RFC 8292).

  Keys are passed around as unpadded base64url strings, as browsers give
  and want them: a public key is an uncompressed P-256 point (65 bytes)
  and a private key its 32 byte scalar."
  (:require [clojure.data.json :as json]
            [clojure.string :as string])
  (:import [java.math BigInteger]
           [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.nio ByteBuffer]
           [java.nio.charset StandardCharsets]
           [java.security AlgorithmParameters KeyFactory KeyPairGenerator
            SecureRandom Signature]
           [java.security.interfaces ECPrivateKey ECPublicKey]
           [java.security.spec ECGenParameterSpec ECParameterSpec ECPoint
            ECPrivateKeySpec ECPublicKeySpec]
           [java.time Duration]
           [java.util Arrays Base64]
           [javax.crypto Cipher KeyAgreement Mac]
           [javax.crypto.spec GCMParameterSpec SecretKeySpec]))

(defn- b64 ^String [^bytes bs]
  (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bs))

(defn- unb64 ^bytes [^String s]
  ;; browsers sometimes give us padded or plain base64
  (.decode (Base64/getUrlDecoder)
           (-> s (string/replace "+" "-") (string/replace "/" "_") (string/replace "=" ""))))

(defn- utf8 ^bytes [^String s] (.getBytes s StandardCharsets/UTF_8))

(defn- concat-bytes ^bytes [& arrays]
  (let [out (ByteBuffer/allocate (reduce + (map alength arrays)))]
    (doseq [^bytes a arrays] (.put out a))
    (.array out)))

(defn- random-bytes ^bytes [n]
  (let [bs (byte-array n)]
    (.nextBytes (SecureRandom.) bs)
    bs))

(def ^:private ^ECParameterSpec p256
  (let [params (AlgorithmParameters/getInstance "EC")]
    (.init params (ECGenParameterSpec. "secp256r1"))
    (.getParameterSpec params ECParameterSpec)))

(defn- unsigned-32 ^bytes [^BigInteger n]
  ;; a BigInteger's bytes may have a leading sign byte, or be short
  (let [bs (.toByteArray n)
        len (alength bs)]
    (if (= 32 len)
      bs
      (let [out (byte-array 32)]
        (System/arraycopy bs (max 0 (- len 32)) out (max 0 (- 32 len)) (min 32 len))
        out))))

(defn- public-key-bytes ^bytes [^ECPublicKey k]
  (let [w (.getW k)]
    (concat-bytes (byte-array [4])
                  (unsigned-32 (.getAffineX w))
                  (unsigned-32 (.getAffineY w)))))

(defn- ->public-key ^ECPublicKey [^bytes bs]
  (when-not (and (= 65 (alength bs)) (= 4 (aget bs 0)))
    (throw (ex-info "Not an uncompressed P-256 public key" {:length (alength bs)})))
  (.generatePublic
   (KeyFactory/getInstance "EC")
   (ECPublicKeySpec. (ECPoint. (BigInteger. 1 (Arrays/copyOfRange bs 1 33))
                               (BigInteger. 1 (Arrays/copyOfRange bs 33 65)))
                     p256)))

(defn- ->private-key ^ECPrivateKey [^bytes bs]
  (.generatePrivate (KeyFactory/getInstance "EC")
                    (ECPrivateKeySpec. (BigInteger. 1 bs) p256)))

(defn- generate-key-pair []
  (let [g (KeyPairGenerator/getInstance "EC")]
    (.initialize g (ECGenParameterSpec. "secp256r1"))
    (.generateKeyPair g)))

(defn generate-keys
  "Make a new VAPID key pair, as {:public-key :private-key}"
  []
  (let [kp (generate-key-pair)]
    {:public-key  (b64 (public-key-bytes (.getPublic kp)))
     :private-key (b64 (unsigned-32 (.getS ^ECPrivateKey (.getPrivate kp))))}))

(defn- hmac-sha256 ^bytes [^bytes key ^bytes data]
  (let [mac (Mac/getInstance "HmacSHA256")]
    (.init mac (SecretKeySpec. key "HmacSHA256"))
    (.doFinal mac data)))

(defn- hkdf
  "HKDF-SHA256 with output at most one hash long, which is all we need"
  ^bytes [^bytes salt ^bytes ikm ^bytes info n]
  (-> (hmac-sha256 (hmac-sha256 salt ikm) (concat-bytes info (byte-array [1])))
      (Arrays/copyOf (int n))))

(def ^:private record-size 4096)

(defn encrypt
  "Encrypt `plaintext` (bytes) for a subscription with keys `p256dh` and
  `auth` as the aes128gcm content coding of RFC 8291. `salt` and
  `local-key-pair` are random unless given, which is only for testing."
  ^bytes [^bytes plaintext p256dh auth & {:keys [^bytes salt local-key-pair]}]
  (let [ua-public   (unb64 p256dh)
        auth-secret (unb64 auth)
        salt        (or salt (random-bytes 16))
        kp          (or local-key-pair (generate-key-pair))
        as-public   (public-key-bytes (.getPublic kp))
        ecdh-secret (let [ka (KeyAgreement/getInstance "ECDH")]
                      (.init ka (.getPrivate kp))
                      (.doPhase ka (->public-key ua-public) true)
                      (.generateSecret ka))
        ikm   (hkdf auth-secret ecdh-secret
                    (concat-bytes (utf8 "WebPush: info") (byte-array [0]) ua-public as-public)
                    32)
        cek   (hkdf salt ikm (concat-bytes (utf8 "Content-Encoding: aes128gcm") (byte-array [0])) 16)
        nonce (hkdf salt ikm (concat-bytes (utf8 "Content-Encoding: nonce") (byte-array [0])) 12)
        cipher (doto (Cipher/getInstance "AES/GCM/NoPadding")
                 (.init Cipher/ENCRYPT_MODE
                        (SecretKeySpec. cek "AES")
                        (GCMParameterSpec. 128 nonce)))
        ;; a single record, so it ends with the last-record delimiter
        ciphertext (.doFinal cipher (concat-bytes plaintext (byte-array [2])))]
    (when (> (+ (alength ciphertext) 86) record-size)
      (throw (ex-info "Push message too long" {:length (alength plaintext)})))
    (concat-bytes salt
                  (-> (ByteBuffer/allocate 4) (.putInt record-size) (.array))
                  (byte-array [(count as-public)])
                  as-public
                  ciphertext)))

(defn vapid-header
  "The Authorization header for a push to `endpoint`, signed with `keys`.
  `subject` is a mailto: or https: URL push services can contact us at."
  [endpoint {:keys [public-key private-key]} subject]
  (let [uri (URI. endpoint)
        audience (str (.getScheme uri) "://" (.getAuthority uri))
        part #(b64 (utf8 (json/write-str %)))
        unsigned (str (part {:typ "JWT" :alg "ES256"})
                      "."
                      (part {:aud audience
                             :exp (+ (quot (System/currentTimeMillis) 1000) (* 12 3600))
                             :sub subject}))
        signature (let [s (Signature/getInstance "SHA256withECDSAinP1363Format")]
                    (.initSign s (->private-key (unb64 private-key)))
                    (.update s (utf8 unsigned))
                    (.sign s))]
    (str "vapid t=" unsigned "." (b64 signature) ", k=" public-key)))

(defonce ^:private ^HttpClient client
  (delay (-> (HttpClient/newBuilder)
             (.connectTimeout (Duration/ofSeconds 10))
             (.build))))

(defn send!
  "Send `message`, which will be JSON encoded, to `subscription`, a map
  with :endpoint :p256dh and :auth. Returns the push service's HTTP
  status; 404 or 410 mean the subscription has gone away for good.
  `ttl` is how many seconds the push service should keep trying for."
  [{:keys [endpoint p256dh auth]} message & {:keys [keys subject ttl] :or {ttl 86400}}]
  (let [body (encrypt (utf8 (json/write-str message)) p256dh auth)
        request (-> (HttpRequest/newBuilder (URI. endpoint))
                    (.timeout (Duration/ofSeconds 30))
                    (.header "Authorization" (vapid-header endpoint keys subject))
                    (.header "Content-Encoding" "aes128gcm")
                    (.header "Content-Type" "application/octet-stream")
                    (.header "TTL" (str ttl))
                    (.POST (HttpRequest$BodyPublishers/ofByteArray body))
                    (.build))]
    (.statusCode (.send ^HttpClient @client request (HttpResponse$BodyHandlers/discarding)))))

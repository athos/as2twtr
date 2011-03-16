(ns as2twtr.ssl
  (use [clojure.contrib.import-static :only (import-static)])
  (import [javax.net.ssl X509TrustManager SSLContext HostnameVerifier]))

(import-static javax.net.ssl.HttpsURLConnection
  getDefaultSSLSocketFactory
  setDefaultSSLSocketFactory
  getDefaultHostnameVerifier
  setDefaultHostnameVerifier)

(declare
  SSL_DEFAULT_CONTEXT
  SSL_NONCERTIFIED_CONTEXT
  call-with-ssl-context
  with-ssl-context
  make-ssl-socket
  )

(def SSL_DEFAULT_CONTEXT {:client-checker nil, :server-checker nil
			  :accepted-issuers nil, :hostname-verifier nil})
(def SSL_NONCERTIFIED_CONTEXT {:client-checker (fn [_ _] nil)
			       :server-checker (fn [_ _] nil)
			       :accepted-issuers (fn [] nil)
			       :hostname-verifier (fn [_ _] true)})

(def #^{:private true} *ssl-context* SSL_DEFAULT_CONTEXT)

(defn- make-trust-manager []
  (reify X509TrustManager
    (checkClientTrusted [this certificates auth-type]
      ((:client-checker *ssl-context*) certificates auth-type))
    (checkServerTrusted [this certificates auth-type]
      ((:server-checker *ssl-context*) certificates auth-type))
    (getAcceptedIssuers [this]
      ((:accepted-issuers *ssl-context*)))))

(defn- make-socket-factory []
  (let [context (SSLContext/getInstance "SSL")]
    (.init context nil (into-array [(make-trust-manager)]) nil)
    (.getSocketFactory context)))

(defn- make-hostname-verifier []
  (reify HostnameVerifier
    (verify [this hostname session]
      ((:hostname-verifier *ssl-context*) hostname session))))

(defn call-with-ssl-context [context proc]
  (let [{client-checker :client-checker
	 server-checker :server-checker
	 accepted-issuers :accepted-issuers
	 hostname-verifier :hostname-verifier}
	context
	checkers? (and client-checker server-checker accepted-issuers)
	verifier? (not (not hostname-verifier))]
    (when (and (not checkers?)
	       (or client-checker server-checker accepted-issuers))
      (throw (Exception. (str "if one of :client-checker, :server-checker, "
			      "or :accepted-issuer is specified, then all of"
			      "them must be specified"))))
    (binding [*ssl-context* context]
      (let [old-factory (getDefaultSSLSocketFactory)
	    old-verifier (getDefaultHostnameVerifier)]
	(try
	  (when checkers?
	    (setDefaultSSLSocketFactory (make-socket-factory)))
	  (when verifier?
	    (setDefaultHostnameVerifier (make-hostname-verifier)))
	  (proc)
	  (finally
	   (when checkers?
	     (setDefaultSSLSocketFactory old-factory))
	   (when verifier?
	     (setDefaultHostnameVerifier old-verifier))))))))

(defmacro with-ssl-context
  "Evaluates exprs in the specified SSL context.

   Example:
      (with-ssl-context SSL_NONCERTIFIED_CONTEXT
        (.openConnection (java.net.URL. \"https://www.example.com\")))

   This makes it possible to connect to non-certified servers via SSL.

   Note: This macro isn't thread-safe. Use locks for mutual exclusion."
  [context & body]
  `(call-with-ssl-context ~context
     (fn [] ~@body)))

(defn make-ssl-socket [host port]
  (let [factory (getDefaultSSLSocketFactory)]
    (.createSocket factory host port)))

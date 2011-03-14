(ns as2twtr.ssl
  (import [javax.net.ssl X509TrustManager SSLContext]))

(declare
  make-ssl-socket-factory
  make-ssl-socket
  )

(defn- make-nonchecking-trust-manager [& {client-checker :client-checker
					  server-checker :server-checker
					  accepted-issuers :accepted-issuers}]
  (let [client-checker (or client-checker (fn [_ _] nil))
	server-checker (or server-checker (fn [_ _] nil))
	accepted-issuers (or accepted-issuers (fn [] nil))]
    (reify X509TrustManager
      (checkClientTrusted [this certificates auth-type]
	(client-checker certificates auth-type))
      (checkServerTrusted [this certificates auth-type]
	(server-checker certificates auth-type))
      (getAcceptedIssuers [this]
	(accepted-issuers)))))

(defn make-ssl-socket-factory [& opts]
  (let [tmanager (apply make-nonchecking-trust-manager opts)
	context (SSLContext/getInstance "SSL")]
    (.init context nil (into-array [tmanager]) nil)
    (.getSocketFactory context)))

(defn make-ssl-socket [host port & [factory]]
  (let [factory (or factory (make-ssl-socket-factory))]
    (.createSocket factory host port)))

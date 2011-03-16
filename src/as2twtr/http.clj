(ns as2twtr.http
  (use [clojure.contrib.http.connection
	:only (http-connection send-request-entity)]
       [clojure.contrib.io :only (slurp* *default-encoding*)]
       [clojure.string :only (join upper-case)]
       [clojure.contrib.def :only (defnk)])
  (import [java.net URLEncoder CookieHandler CookieManager]
	  [java.io InputStreamReader]))

(declare
  http-request
  )

(defn- query-string [kv-map]
  (join \&
    (for [[key value] kv-map]
      (format "%s=%s"
	      (name key)
	      (URLEncoder/encode (str value) *default-encoding*)))))

(defn- ensure-cookie []
  (when-not (CookieHandler/getDefault)
    (CookieHandler/setDefault (CookieManager.))))

(defn- add-query [url query]
  (str (if (string? url) url (.toString url))
       \?
       (if (string? query) query (query-string query))))

(defn- header-fields->map [fields]
  (into {}
	(for [field fields]
	  [(.getKey field) (vec (.getValue field))])))

(defmethod send-request-entity clojure.lang.APersistentMap [conn entity]
  (send-request-entity conn (query-string entity)))

(defn- request  [url method headers body flusher opts]
  (let [conn (http-connection (if opts (add-query url opts) url))
	method (upper-case (name method))]
    (.setRequestMethod conn method)
    (doseq [[header value] headers]
      (.setRequestProperty conn (str header) (str value)))
    (cond body
	  #_=> (do (.setDoOutput conn true)
		   (send-request-entity conn body))
	  (and (or (= method "POST") (= method "PUT"))
	       opts)
	  #_=> (do (.setDoOutput conn true)
		   (send-request-entity conn opts)))
    (let [headers (header-fields->map (.getHeaderFields conn))]
      [(.getResponseCode conn)
       headers
       (flusher headers (.getInputStream conn))])))

(defnk http-request [url :method :get :headers {}
		     :body nil :encoding *default-encoding*
		     :flusher (fn [headers in]
				(slurp*
				  (InputStreamReader. in *default-encoding*)))
		     :options nil]
  (ensure-cookie)
  (binding [*default-encoding* encoding]
    (let [[status _ :as result]
	  (request url method headers body flusher options)]
      (when-not (= status 200)
	(throw (Exception. "HTTP request failed")))
      result)))

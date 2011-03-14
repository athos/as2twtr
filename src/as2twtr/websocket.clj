(ns as2twtr.websocket
  (use [clojure.contrib.io :only (*byte-array-type*)]
       [clojure.string :only (join split lower-case)])
  (require [as2twtr.ssl :as ssl])
  (import [java.net URI Socket]
          [java.io OutputStream BufferedInputStream BufferedOutputStream]
          [java.security MessageDigest]
          [java.nio ByteBuffer]))

(declare
  make-websocket-client
  disconnect
  string->bytes
  bytes->string
  send-frame
  recv-frame
  handshake
  call-with-websocket-client)

;; websocket client
(defrecord WebSocketClient [socket uri origin in out])

(defn websocket-scheme [^URI uri]
  (let [scheme (.getScheme uri)]
    (when-not (or (= scheme "ws")
		  (= scheme "wss"))
      (java.net.MalformedURLException.))
    scheme))

(defn- websocket-port [^URI uri]
  (let [port (.getPort uri)]
    (if-not (= (.getPort uri) -1)
      port
      (case (websocket-scheme uri)
	"ws" 80
	"wss" 443))))

(defn make-websocket-client [uri & {origin :origin}]
  (let [uri (if (string? uri) (URI. uri) uri)
	host (.getHost uri)
	origin (or origin
		   (str "http://" (.getHost uri))) ; FIXME
	scheme (websocket-scheme uri)
	port (websocket-port uri)]
    (let [socket (if (= scheme "ws")
		   (Socket. host port)
		   (ssl/make-ssl-socket host port))]
      (WebSocketClient. socket
			uri
			origin
			(BufferedInputStream. (.getInputStream socket))
			(BufferedOutputStream. (.getOutputStream socket))))))

;; string <-> byte array conversion
(defn string->bytes [^String s]
  (.getBytes s "UTF-8"))

(defn bytes->string
  ([bs] (bytes->string bs (count bs)))
  ([bs size]
   (String. (bytes bs) 0 (int size) "UTF-8")))

;; functions for sending/receiving frames
(defn- send-data [^OutputStream out, data]
  (cond (string? data)
        #_=> (.write out (bytes (string->bytes data)))
        (integer? data)
        #_=> (.write out (int data))
        (seq? data)
        #_=> (doseq [b data]
               (.write out (int b)))
        (= (class data) *byte-array-type*)
        #_=> (.write out (bytes data)))
  (.flush out))

(defn send-frame [client data]
  (doto (:out client)
    (send-data 0)
    (send-data data)
    (send-data 255))
  nil)

(defn- int->byte [x]
  (let [MAX Byte/MAX_VALUE]
    (byte (or (and (<= x MAX) x)
              (- x (* 2 (+ MAX 1)))))))

(defn- recv-until [in pred]
  (let [buf (java.io.ByteArrayOutputStream.)]
    (loop []
      (let [b (.read in)]
	(if (or (= b -1) (pred b))
	  (.toByteArray buf)
	  (do
	    (.write buf (int (int->byte b)))
	    (recur)))))))

(defn recv-frame [client]
  (when-not (= (.read (:in client)) 0)
    ;; FIXME: more appropriate exception should be thrown
    (throw (Exception. "frame without \\x00 prefix received")))
  (recv-until (:in client) #(= % 255)))

(defn disconnect [client]
  (send-data (:out client) [255 0])
  (.close (:socket client)))

;; handshake
(defn- handshake-request-header [^URI uri, origin, key1, key2]
  (let [path (str (if (= (.getRawPath uri) "")
		    "/"
		    (.getRawPath uri))
                  (if (.getRawQuery uri)
                    (str \? (.getRawQuery uri))
                    ""))]
    (apply str "GET " path " HTTP/1.1\r\n"
               "Upgrade: WebSocket\r\n"
               "Connection: Upgrade\r\n"
               "Host: " (.getAuthority uri) "\r\n"
               "Origin: " origin "\r\n"
               "Sec-WebSocket-Key1: " key1 "\r\n"
               "Sec-WebSocket-Key2: " key2 "\r\n")))

(defn- make-key []
  (let [x (long (rand (Math/pow 2 32)))
        y (inc (rand-int 12))]
    [(* (quot x y) y) y]))

(defn- make-body []
  (into-array Byte/TYPE
              (map (fn [_] (int->byte (rand-int 256)))
                   (range 8))))

(defn- encode [key ns]
  (let [chars (str "!\"#$%&'()*+,-./:;<=>?@ABCDEFGHIJKLMNOPQRST"
                   "UVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")]
    (letfn [(random-char []
              (get chars (rand-int (count chars))))
            (random-chars []
              (map (fn [_] (random-char)) (range (inc (rand-int 12)))))
            (merge [x y]
              (lazy-seq
                (cond (empty? x) y
                      (empty? y) x
                      (< (rand) 0.5)
                      #_=> (cons (first x) (merge (rest x) y))
                      :else
                      #_=> (cons (first y) (merge x (rest y))))))]
      (let [key (str key)
            start (get key 0)
            end (get key (dec (count key)))
            mid (subs key 1 (dec (count key)))]
        (join (merge `(~start ~@(merge mid (repeat ns \space)) ~end)
                     (random-chars)))))))

(defn- send-handshake-request [client]
  (let [[key1 ns1] (make-key)
        [key2 ns2] (make-key)
        body (make-body)
        header (handshake-request-header (:uri client)
					 (:origin client)
					 (encode key1 ns1)
					 (encode key2 ns2))]
    (doto (:out client)
      (send-data header)
      (send-data "\r\n")
      (send-data body))
    [(/ key1 ns1) (/ key2 ns2) body]))

(defn- recv-handshake-response [client]
  (letfn [(recv-fields [in]
            (loop [fields []]
              (let [field (bytes->string (recv-until in #(= % (int \return))))]
                (.read in)  ; omit a newline
                (if (= field "")
                  fields
                  (recur (conj fields field))))))
          (fields->map [fields]
            (reduce (fn [m field]
                      (let [[name value] (split field #": ")]
                        (conj m [(lower-case name) value])))
                    {}
                    fields))]
    (let [fields (recv-fields (:in client))
          body (byte-array 16)]
      (when-not (= (.read (:in client) body) 16)
        ;; FIXME
        (throw (Exception.)))
      [(fields->map (rest fields)) body])))

(defn- challenge-response [key1 key2 body]
  (let [buf (ByteBuffer/allocate 16)
        md5 (MessageDigest/getInstance "MD5")]
    (doto buf
      (.putInt key1)
      (.putInt key2)
      (.put body))
    (.update md5 (.array buf))
    (.digest md5)))

(defn handshake [client]
  (let [[key1 key2 body] (send-handshake-request client)
        answer (challenge-response key1 key2 body)
        [header body] (recv-handshake-response client)]
    (when-not (= answer body)
      ;; FIXME
      (Exception.))
    header))

;; convenient API
(defn call-with-websocket-client [uri proc & {origin :origin}]
  (let [client (make-websocket-client uri :origin origin)]
    (try
      (handshake client)
      (proc client)
      ;; FIXME: more appropriate exception handling
      (catch Exception e
	(throw e))
      (finally
        (when-not (.isClosed (:socket client))
	  (disconnect client))))))

(comment
(letfn [(prompt [p]
          (print p)
          (flush)
          (read-line))]
  (calll-with-websocket-client
    "ws://localhost:8080/"
    (fn [client]
      (loop []
        (let [line (prompt "> ")]
          (when line
            (send-frame client line)
            (println (bytes->string (recv-frame client)))
            (recur)))))))
)

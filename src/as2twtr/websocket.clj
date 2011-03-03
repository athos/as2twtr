(ns as2twtr.websocket
  (use [clojure.contrib.io :only (*byte-array-type*)]
       [clojure.string :only (join split lower-case)])
  (import [java.net URI Socket]
          [java.io BufferedInputStream BufferedOutputStream]
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
(defrecord WebSocketClient [socket uri in out])

(defn- websocket-port [uri]
  (or (.getPort uri)
      (case (.getScheme uri)
	"ws" 80
	"wss" 443
	(java.net.MalformedURLException.))))

(defn make-websocket-client [uri]
  (let [uri (if (string? uri) (URI. uri) uri)
	port (websocket-port uri)]
    (let [socket (Socket. (.getHost uri) port)]
      (WebSocketClient. socket
			uri
			(BufferedInputStream. (.getInputStream socket))
			(BufferedOutputStream. (.getOutputStream socket))))))

(defn disconnect [client]
  (.close (:socket client)))

;; string <-> byte array conversion
(defn string->bytes [s]
  (.getBytes s "UTF-8"))

(defn bytes->string
  ([bs] (bytes->string bs (count bs)))
  ([bs size]
   (String. bs 0 size "UTF-8")))

;; functions for sending/receiving frames
(defn- send-data [out data]
  (cond (string? data)
        #_=> (.write out (string->bytes data))
        (integer? data)
        #_=> (.write out data)
        (seq? data)
        #_=> (doseq [b data]
               (.write out b))
        (= (class data) *byte-array-type*)
        #_=> (.write out data))
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
  (loop [bs []]
    (let [b (.read in)]
      (if (or (= b -1) (pred b))
        (into-array Byte/TYPE bs)
        (recur (conj bs (int->byte b)))))))

(defn recv-frame [client]
  (when-not (= (.read (:in client)) 0)
    ;; FIXME: more appropriate exception should be thrown
    (throw (Exception. "frame without \\x00 prefix received")))
  (recv-until (:in client) #(= % 255)))

;; handshake
(defn- handshake-request-header [^URI uri, key1, key2]
  (let [path (str (.getRawPath uri)
                  (if (.getRawQuery uri)
                    (str \? (.getRawQuery uri))
                    ""))]
    (apply str "GET " path " HTTP/1.1\r\n"
               "Upgrade: WebSocket\r\n"
               "Connection: Upgrade\r\n"
               "Host: " (.getAuthority uri) "\r\n"
               "Origin: http://" (.getHost uri) "\r\n"
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
(defn call-with-websocket-client [uri proc]
  (let [client (make-websocket-client uri)]
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

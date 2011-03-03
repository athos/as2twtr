(ns as2twtr.as-chat
  (use [clojure.contrib.json :only (read-json)])
  (require [as2twtr.websocket :as ws]))

(declare
  make-chat-client
  connect
  disconnect
  post-message
  get-message
  )

;; chat message content
(defrecord Content [id name body])

(defn- message->content [message]
  (let [content (:content message)]
    (Content. (:id content) (:name content) (:body content))))

(defn- message-type [message]
  (:event message))

;; chat client
(defrecord ChatClient [websocket post-uri observer-thread])

(defn make-chat-client [websocket-uri post-uri]
  (ChatClient. (ws/make-websocket-client websocket-uri) post-uri (atom nil)))

(defn- make-handler [client observer]
  (let [handle-it (or observer (fn [_ _] nil))]
    (letfn [(iter []
	      (let [message (get-message client)]
		(handle-it client
			   (message-type message)
			   (message->content frame))
		(recur)))]
      (.start (Thread. iter)))))

(defn connect [client & [observer]]
  (handshake (:websocket client))
  (reset! (:observer-thread client) (make-handler client observer))
  client)

(defn disconnect [client]
  (doto (:observer-thread client)
    (.stop)
    (reset! nil))
  client)

;; post message
(defn post-message [client message]
  nil)

;; get message (synchronously)
(defn get-message [client]
  (-> client
      ws/recv-frame
      ws/bytes->string
      read-json))

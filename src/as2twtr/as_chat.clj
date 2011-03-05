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
  (keyword (:event message)))

;; chat client
(defrecord ChatClient [websocket post-uri observer-thread])

(defn- make-handler [client observer]
  (let [handle-it (or observer (fn [_ _] nil))]
    (letfn [(iter []
	      (let [message (get-message client)]
		(handle-it (message-type message) (message->content message))
		(recur)))]
      (doto (Thread. iter)
	(.start)))))

(defn connect [websocket-uri post-uri & [observer]]
  (let [client (ChatClient. (ws/make-websocket-client websocket-uri)
			    post-uri
			    (atom nil))]
    (ws/handshake (:websocket client))
    (reset! (:observer-thread client) (make-handler client observer))
  client))

(defn disconnect [client]
  (.stop @(:observer-thread client))
  (reset! (:observer-thread client) nil)
  (ws/disconnect (:websocket client))
  nil)

;; post message
(defn post-message [client message]
  nil)

;; get message (synchronously)
(defn get-message [client]
  (-> (:websocket client)
      ws/recv-frame
      ws/bytes->string
      read-json))

(ns as2twtr.as-chat
  (use [as2twtr.http :only (http-request)]
       [clojure.contrib.json :only (read-json)])
  (require [as2twtr.websocket :as ws]))

(declare
  make-chat-client
  connect
  disconnect
  login
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
(defrecord ChatClient [user password websocket api-base-uri observer-thread])

(defn- make-handler [client observer]
  (let [handle-it (or observer (fn [_ _] nil))]
    (letfn [(iter []
	      (let [message (get-message client)]
		(handle-it (message-type message) (message->content message))
		(recur)))]
      (doto (Thread. iter)
	(.start)))))

(defn connect [user password room-id websocket-uri api-base-uri & [observer]]
  (let [client (ChatClient. user
			    password
			    (ws/make-websocket-client websocket-uri)
			    api-base-uri
			    (atom nil))]
    (ws/handshake (:websocket client))
    (when observer
      (reset! (:observer-thread client) (make-handler client observer)))
  client))

(defn disconnect [client]
  (.stop @(:observer-thread client))
  (reset! (:observer-thread client) nil)
  (ws/disconnect (:websocket client))
  nil)

(defn- api-request [client path & {query :query, body :body}]
  (let [base-uri (:api-base-uri client)]
    (if body
      (http-request (str base-uri path)
	:method :post
	:options query
	:body body)
      (http-request (str base-uri path) :options query))))

;; login
(defn login [client]
  (api-request
    client "/login"
    :query {:user (:user client), :password (:password client)}))

;; post message
(defn post-message [client message]
  (api-request
    client "/message"
    :body {:room_id (:room-id client), :message message}))

;; get message (synchronously)
(defn get-message [client]
  (-> (:websocket client)
      ws/recv-frame
      ws/bytes->string
      read-json))

(ns as2twtr.twitter
  (use as2twtr.config)
  (import [twitter4j Twitter Paging]
	  [twitter4j.http AccessToken]))

(defn make-twitter-client [consumer-key consumer-secret access-token access-token-secret]
  (doto (Twitter.)
    (.setOAuthConsumer consumer-key consumer-secret)
    (.setOAuthAccessToken (AccessToken. access-token access-token-secret))))

(defn update-status [client msg]
  (.updateStatus client msg))

(defn in-reply-to [client status]
  (let [reply-id (.getInReplyToStatusId status)]
    (if (= reply-id -1)
      nil
      (-> client (.showStatus reply-id) (.getText)))))

(defn mentions
  ([client] (mentions client nil))
  ([client since-id]
   (let [ms (if since-id
	      (.getMentions client (Paging. (long since-id)))
	      (.getMentions client))]
     (sort #(>= (first %1) (first %2))
	   (for [m ms]
	     (let [u (.getUser m)]
	       [(.getId m)
		(.getText m)
		(.getScreenName u)
		(.getId u)
		(in-reply-to m)]))))))

(defn followers [client]
  (-> client (.getFriendsIDs) (.getIDs) seq))

(defn max-status-id [ms]
  (if (empty? ms) false (first (first ms))))
